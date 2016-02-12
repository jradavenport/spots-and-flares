pro anim, noplot=noplot
set_plot,'X'
plotstuff,/set,/silent

; GOAL:
; - spinning globe w/ starspots & flares
;   while simultaneously a real light curve is being generated
; - do few versions with different spot configurations


stsp_prefix = 'multispot2'

; flare rate is a number between 0-1. gives the acceptance rate for
; randomly generating a new flare per frame
flare_rate = 0.2 ; set to 0 for NO flares

nframes = 360 ; use this also as the rotation period
extra_rot = 10 ; number of extra rotations to generate data over

incl = 12 ; stellar inclination
rot = 24 ; rotation (position) angle. no reason, just looks cool

Eslope = -2.0 ; powerlaw slope
Emin = 0.0005 ; min flare energy
Emax = 0.01 ; max flare energy

followspot = 1

; make flares around the equator
eqband = 'no'
eqrange = 20. ; if "yes", this is the latitude band width

extraspots = 0. ; number of extra little spots to draw, not included in STSP model (limit usually 10)


; read the starspot VIZ outputs from STSP
readcol, stsp_prefix + '.in', inraw, f='(F)', /silent
if inraw[0] eq 0 then $ ; if no planets
    nspots = inraw[8]
readcol, stsp_prefix + '_lcout.txt', time_0, flux_0, f='(F,X,X,F)', /silent


time = time_0
for k=1,extra_rot do $
    time = [time, time_0+max(time)]
flux = flux_0
for k=1,extra_rot do $
    flux = [flux, flux_0]

flux_orig = flux ; might be useful later...

; read the spot positions from the .in file
rad = inraw[n_elements(inraw) - (findgen(nspots)*3.+4.)]
lat = inraw[n_elements(inraw) - (findgen(nspots)*3.+3.)]/!dpi*180.-90.
lon = inraw[n_elements(inraw) - (findgen(nspots)*3.+2.)]/!dpi*180.

; hold the flare parameters:
; [tpeak, fwhm, ampl, t_created, lon, lat]
flare_params = [-1, -1, -1, -1, -1, -1]
; flares are tracked from -1 FWHM to 10 FWHM
fwhm_max = 10.

set_plot,'ps'
FL_out = [-1]

; set the max number of frames to actually plot (default is, of course Nframes)
nframes_max = nframes
; unless we want to plot NONE...
if KEYWORD_SET(noplot) then $
    nframes_max = -1

FOR i=0l,nframes + (nframes * extra_rot)-1 DO BEGIN
    if i LT nframes_max then begin
        device, filename='img/frame'+string(i,f='(I05)')+'.eps',$
            /encap,/inch,/color,xsize=6,ysize=8

        ; draw the globe
        loadct,0, /silent
        map_set, /satel, sat_p=[1d8,0,rot], incl, -i, /noborder, /grid, /horizon, $
              xmarg=0, ymarg=0, glinethick=3, color=50, $
              position = posgen(1,4,1,ysp=3, xmin=0.02,xmax=0.98, $
                                ymin=0.02,ymax=0.98)
    endif

    ; draw the spots
    for k=0,nspots-1 do begin
        drawcircle, lon[k], lat[k], rad[k], xxc, yyc
        if i LT nframes_max then $
            polyfill, xxc, yyc, color=170
        oplot, [xxc,xxc[0]], [yyc, yyc[0]], thick=2,color=50
    endfor

    ; draw the flares
    loadct, 39, /silent

     ; should we make a new flare?
     new_fl_p = randomu(sss,1)
     if new_fl_p le flare_rate then begin
        ; use random powerlaw to draw flare energy
        Eng = RANDOMP(sss, 1, Eslope, min=Emin, max=Emax)

        fwhm = alog10(Eng)+4. ; replace these w/ probabilities later
        ampl = Eng ;0.002

        ; flare position properties...
        IF NOT KEYWORD_SET(followspot) THEN BEGIN
            ; FOR RANDOM FLARES:
            ;  - randomly within observed lon range
            flare_lon = -i + randomu(sss, 1) * 180. - 90.

            ;  - gaussian lat range within equatorial band
            if eqband eq 'yes' then $
                flare_lat = randomn(sss,1) * eqrange

            ;  - random latitudes on whole surface
            if eqband eq 'no' then $
                flare_lat = randomu(sss,1) * 180. - 90.
        ENDIF

        ; FOR FOLLOWSPOT FLARES:
        ;  - use gaussian kernel in lat and lon around spot(s)
        IF KEYWORD_SET(followspot) THEN BEGIN
            ; pick a random spot to put flare near
            x = floor(randomu(sss,1) * (n_elements(lon) + 1))

            ; find random position for the flare within the spot
            coord_tmp = flarecircle(lon[x], lat[x], rad[x], sss)
            flare_lon = coord_tmp[0]
            flare_lat = coord_tmp[1]
        ENDIF

        ; find great circle distance from center of star to flare
        fdist = map_2points(-i, incl, flare_lon, flare_lat)

        ; if flare not over the horizon, add it
        if fdist[0] le 90 then begin
            ;add the flare to the flare properties array
            ptmp = [i+fwhm, fwhm, ampl, i, flare_lon, flare_lat]
            flare_params = [[flare_params], [ptmp]]

            ; then add the flare to the light curve
            flux = flux + real(aflare(time, ptmp[0:2]),0)
            FL_out = [FL_out, i]
        endif

     endif


     if (n_elements(flare_params)/6.) gt 1 then begin
        ; look for flares still to be tracked
        x = where(i lt (flare_params[0,1:*] + flare_params[1,1:*] * fwhm_max))
        tmp = [-1, -1, -1, -1, -1, -1]
        if x[0] ne -1 then begin
           ; keep flares still to be tracked
           for k=0,n_elements(x)-1 do $
              tmp = [[tmp], [flare_params[*,x[k]+1]]]
        endif
        flare_params = tmp
     endif

    if i LT nframes_max then begin
         if (n_elements(flare_params)/6.) gt 1 then begin
         ;oplot, flare_lon, flare_lat, psym=4, color=250
            for k=1l,(n_elements(flare_params)/6.)-1 do begin
                loadct,0,/silent
                oplot, [flare_params[4,k]], [flare_params[5,k]], $
                     psym=8, symsize=2.4,color=50
                loadct,39,/silent
                oplot, [flare_params[4,k]], [flare_params[5,k]], $
                     psym=8, color=225, symsize=2.
            endfor

         endif

        ; draw the light curve
        yrng = [min(flux), max(flux)]

        plot, [time[0:i]], [flux[0:i]],xsty=9,ysty=9, $
            position=posgen(1,4,4, xmin=0.1,xmax=0.95, ymin=0.05,ymax=0.7), /noerase,$
            xtitle='Time', ytitle='Flux', charsize=0.8, xtickname=replicate(' ',8),$
            xrange=[0,nframes], yrange=yrng

        device,/close
        spawn,'convert -density 150x150 -flatten img/frame' + $
           string(i,f='(I05)')+'.eps img/frame'+string(i,f='(I05)')+'.jpeg'
        spawn, 'rm img/*.eps'
    endif

  ENDFOR


followspot_out = 'no'
if KEYWORD_SET(followspot) then $
followspot_out = 'yes'

;-- generate a LOG FILE to save params used
openw, 1, stsp_prefix + '_animparams.txt'
printf, 1, 'Created on ' + systime()
printf, 1, '  using these parameters:'

printf, 1, 'stsp_prefix = ' + string(stsp_prefix)
printf, 1, 'flare_rate = ' + string(flare_rate)
printf, 1, 'nframes = ' + string(nframes)
printf, 1, 'extra_rot = ' + string(extra_rot)
printf, 1, 'incl = ' + string(incl)
printf, 1, 'rot = ' + string(rot)
printf, 1, 'Eslope = ' + string(Eslope)
printf, 1, 'Emin = ' + string(Emin)
printf, 1, 'Emax = ' + string(Emax)
printf, 1, 'followspot = ' + string(followspot_out)
printf, 1, 'eqband = ' + string(eqband)
printf, 1, 'eqrange = ' + string(eqrange)
; printf, 1, '' +
close, 1


if not KEYWORD_SET(noplot) then begin
    ; render movie
    spawn,'ffmpeg -i img/frame%05d.jpeg -pix_fmt yuv420p -r 30 -qscale 1 ' + $
        stsp_prefix + '.mp4'
endif

loadct,0,/silent

; finally, produce phase-folded plot
device,filename= stsp_prefix+'_phaseplot.eps',/encap,/color,/inch,xsize=5,ysize=9
contour_plus, (time mod nframes)/nframes, flux, 1, position=posgen(1,2,2), $
    xrange=[0,1], /xsty, /ysty, xtitle='Phase', ytitle='Flux', charsize=1,$
    /pixel,/reverse, xbin=0.01, ybin=(max(flux)-min(flux))/100.
plothist, (float(fl_out) mod nframes)/nframes,bin=0.05,/half, $
    position=posgen(1,2,1),/noerase, xtickname=replicate(' ',8), $
    xrange=[0,1], /xsty, ytitle='# Flares', charsize=1,thick=4,/fill,/fline,forient=45
device,/close


set_plot,'X'
stop
return
end


pro spotgen, nspots, maxrad=maxrad
    ; a short helper script to generate random spot sizes/positions
    ; use to copy/paste into STSP input file

    if not keyword_set(maxrad) then $
        maxrad = 0.1
    if not keyword_set(nspots) then $
     nspots = 5

    lon = randomu(sss,nspots)*2.*!dpi
    lat = randomn(sss,nspots)*(!dpi/2.)/5. + !dpi/2.
    rad = randomu(sss,nspots)*maxrad

    print, 'COPY THIS IN TO STSP...'
    for i=0l,nspots-1 do print,rad[i],lat[i],lon[i],f='(F5.3)'
    return
end


pro drawcircle, lon_in, lat_in, rad_in,$
    gl, gb, plot=plot, _Extra=e ;lon_out,lat_out
  ;-- enter the coords and size of a spot:
  ;   LON_in, LAT_in, RAD_in
  ;  -> everything in degrees
  ;
  ;-- code "puts" RAD_in circle about the pole, rotates coordinates to
  ;   place at desired (lat,lon), using math stolen from GLACTC.pro

radeg = 180.0d/!DPI
rapol = lon_in/15.
decpol = lat_in
posang,0,0,!pi,lon_in*!dtor,lat_in*!dtor,dlon ;-- compute position angle
dlon = 0 ; (dlon/!dtor) mod 180.

gl = fltarr(1000)
gb = fltarr(1000)

ra = dindgen(1000)/999.*360.  ; make ring of points with RAD_in about pole
dec= fltarr(1000)*0.+ 90. - asin(rad_in)/!dtor

sdp = sin(decpol/radeg)
cdp = sqrt(1.0d0-sdp*sdp)
radhrs=radeg/15.0d0

ras  = ra
decs = dec

ras = ras/radeg - rapol/radhrs
sdec = sin(decs/radeg)
cdec = sqrt(1.0d0-sdec*sdec)
sgb = sdec*sdp + cdec*cdp*cos(ras)
gb = radeg * asin(sgb)
cgb = sqrt(1.0d0-sgb*sgb)
sine = cdec * sin(ras) / cgb
cose = (sdec-sdp*sgb) / (cdp*cgb)
gl = dlon - radeg*atan(sine,cose)+lon_in
ltzero = where(gl lt 0.0, Nltzero)
if Nltzero ge 1 then gl[ltzero]=gl[ltzero]+360.0d0

if keyword_set(plot) then oplot,gl,gb, _Extra=e
return
end



function flarecircle, lon_in, lat_in, rad_in, seed
; using same math as drawcircle, pick a random location within a starspot
; uniform random in (r,theta) within the starspot
radeg = 180.0d/!DPI
rapol = lon_in/15.
decpol = lat_in
posang, 0, 0, !pi, lon_in*!dtor, lat_in*!dtor, dlon ;-- compute position angle
dlon = 0 ; (dlon/!dtor) mod 180.

gl = fltarr(1000)
gb = fltarr(1000)

rad_rand = randomu(seed,1) * rad_in
ra = randomU(seed,1)*360.  ; make ring of points with RAD_in about pole
dec= 90. - asin(rad_rand)/!dtor

sdp = sin(decpol/radeg)
cdp = sqrt(1.0d0-sdp*sdp)
radhrs=radeg/15.0d0

ras  = ra
decs = dec

ras = ras/radeg - rapol/radhrs
sdec = sin(decs/radeg)
cdec = sqrt(1.0d0-sdec*sdec)
sgb = sdec*sdp + cdec*cdp*cos(ras)
gb = radeg * asin(sgb)
cgb = sqrt(1.0d0-sgb*sgb)
sine = cdec * sin(ras) / cgb
cose = (sdec-sdp*sgb) / (cdp*cgb)
gl = dlon - radeg*atan(sine,cose)+lon_in
ltzero = where(gl lt 0.0, Nltzero)
if Nltzero ge 1 then gl[ltzero]=gl[ltzero]+360.0d0

return, [gl, gb]
end
