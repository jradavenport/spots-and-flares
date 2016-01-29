pro anim, followspot=followspot
  set_plot,'X'
  plotstuff,/set,/silent

  ; GOAL:
  ; - spinning globe w/ starspots & flares
  ;   while simultaneously a real light curve is being generated
  ; - do few versions with different spot configurations

  ; flare rate is a number between 0-1. gives the acceptance rate for
  ; randomly generating a new flare per frame
  flare_rate = 0.5
  
  ; read the starspot VIZ outputs from STSP
  stsp_prefix = 'spot_v2'
  readcol, stsp_prefix + '.in', inraw, f='(F)', /silent
  if inraw[0] eq 0 then $ ; if no planets
     nspots = inraw[8]
  readcol, stsp_prefix + '_lcout.txt', time, flux, f='(F,X,X,F)', /silent

  flux_orig = flux ; might be useful later...

  ; read the spot positions from the .in file
  rad = inraw[n_elements(inraw) - (findgen(nspots)*3.+4.)]
  lat = inraw[n_elements(inraw) - (findgen(nspots)*3.+3.)]/!dpi*180.-90.
  lon = 360 - inraw[n_elements(inraw) - (findgen(nspots)*3.+2.)]/!dpi*180.


  nframes = 360 ; use this also as the rotation period
  incl = 12 ; stellar inclination
  rot = 24 ; rotation (position) angle. no reason, just looks cool

  ; hold the flare parameters:
  ; [tpeak, fwhm, ampl, t_created, lon, lat]
  flare_params = [-1, -1, -1, -1, -1, -1]
  ; flares are tracked from -1 FWHM to 10 FWHM
  fwhm_max = 10.


  set_plot,'ps'

  FOR i=0l,nframes-1 DO BEGIN
     device, filename='img/frame'+string(i,f='(I05)')+'.eps',$
             /encap,/inch,/color,xsize=6,ysize=8

     ; draw the globe
     loadct,0, /silent
     map_set, /satel, sat_p=[1d8,0,rot], incl, -i, /noborder, /grid, /horizon, $
              xmarg=0, ymarg=0, glinethick=3, color=50, $
              position = posgen(1,4,1,ysp=3, xmin=0.02,xmax=0.98, $
                                ymin=0.02,ymax=0.98)

     ; draw the spots
     for k=0,nspots-1 do begin
        drawcircle, lon[k], lat[k], rad[k], xxc, yyc
        polyfill, xxc, yyc, color=90
     endfor


     ; draw the flares
     loadct, 39, /silent

     ; should we make a new flare?
     new_fl_p = randomu(sss,1)
     if new_fl_p le flare_rate then begin
        Eng = randomp(sss, 1, -1, min=0.001, max=0.1)

        fwhm = alog10(Eng)+4 ; replace these w/ probabilities later
        ampl = Eng ;0.002

        ; flare position properties...
        ; FOR RANDOM FLARES:
        ;  - randomly within observed lon range
        ;  - gaussian lat range within equatorial band
        IF NOT KEYWORD_SET(followspot) THEN BEGIN
            flare_lon = -i + randomu(sss, 1)*180.-90.
            flare_lat = randomn(sss,1)*20.

            ptmp = [i+fwhm, fwhm, ampl, i, flare_lon, flare_lat]
            flare_params = [[flare_params], [ptmp]]

            ; add the flare to the light curve
            flux = flux + real(aflare(time, ptmp[0:2]),0)
        ENDIF

        ; FOR FOLLOWSPOT FLARES:
        ;  - use gaussian kernel in lat and lon around spot(s)
        IF KEYWORD_SET(followspot) THEN BEGIN
            ; pick a random spot to put flare near
            x = floor(randomu(sss,1) * (n_elements(lon) + 1))
            ; put flare near spot
            flare_lon = lon[x] + randomn(sss, 1) * rad[x]*90.
            flare_lat = lat[x] + randomn(sss, 1) * rad[x]*90.

            ; find if flare is within view (even if spot is not quite yet)
            sok = (abs((360-i) - flare_lon) LT 90.)
            if sok[0] ne 0 then begin
                ptmp = [i+fwhm, fwhm, ampl, i, flare_lon, flare_lat]
                flare_params = [[flare_params], [ptmp]]

                ; add the flare to the light curve
                flux = flux + real(aflare(time, ptmp[0:2]),0)
            endif
        ENDIF

     endif

     if (n_elements(flare_params)/6.) gt 1 then begin
        ; look for flares still to be tracked
        x = where(i lt (flare_params[0,1:*] + flare_params[1,1:*] * fwhm_max))
        if x[0] ne -1 then begin
           tmp = [-1, -1, -1, -1, -1, -1]
           ; remove flares not being tracked
           for k=0,n_elements(x)-1 do $
              tmp = [[tmp], [flare_params[*,x[k]+1]]]
           flare_params = tmp
        endif
     endif

     if (n_elements(flare_params)/6.) gt 1 then begin
     ;oplot, flare_lon, flare_lat, psym=4, color=250
        for k=1l,(n_elements(flare_params)/6.)-1 do begin
           oplot, [flare_params[4,k]], [flare_params[5,k]], $
                  psym=8, color=250, symsize=2.
        endfor

     endif

     ; draw the light curve

     yrng = [min(flux), max(flux)]

     plot, [time[0:i]], [flux[0:i]],xsty=9,ysty=9, $
           position=posgen(1,4,4, xmin=0.1,xmax=0.95, ymin=0.05,ymax=0.7), /noerase,$
           xtitle='Time', ytitle='Flux', charsize=0.8, xtickname=replicate(' ',8),$
           xrange=[0,nframes], yrange=yrng

     device,/close
     spawn,'convert -density 150x150 -flatten img/frame'+$
           string(i,f='(I05)')+'.eps img/frame'+string(i,f='(I05)')+'.jpeg'
     spawn, 'rm img/*.eps'

  ENDFOR

  set_plot,'X'

  spawn,'ffmpeg -r 20 -i img/frame%05d.jpeg -pix_fmt yuv420p -r 20 -qscale 1 test.mp4'


  stop
  return
end


pro spotgen, nspots, maxrad=maxrad
  if not keyword_set(maxrad) then $
     maxrad = 0.1
  if not keyword_set(nspots) then $
     nspots = 5

  lon = randomu(sss,nspots)*2.*!dpi
  lat = randomn(sss,nspots)*(!dpi/2.)/5. + !dpi/2.
  rad = randomu(sss,nspots)*maxrad

  for i=0l,nspots-1 do print,rad[i],lat[i],lon[i],f='(F5.3)'

  return
end
