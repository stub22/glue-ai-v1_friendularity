:- module(floblocks, [section/2,
		      subsection/2,
		      description/2,
		      visual_style/2,
		      inputs/2,
		      outputs/2,
		      input_types/2,
		      output_types/2,
		     image_name/2,
		     prototype_coordinates/3,
		     icon_size/3]).
/** <module>  Static definitions of block types


*/

:- discontiguous section/2,
	subsection/2,
	description/2,
	visual_style/2,
	inputs/2,
	outputs/2,
	input_types/2,
	output_types/2,
	image_name/2,
	prototype_coordinates/3,
	icon_size/3.

%  ===== if ===========
section(if, 'Control').
subsection(if, 'Control').
description(if, 'If Cond is true, O is A, else O is B').
visual_style(if,  normal).
inputs(if, [required('Cond'), required('A'), required('B')]).
outputs(if, ['O']).
input_types(if, [scalar, object, object]).
output_types(if, [object]).
image_name(if, '/img/blocks/blocksheet0-_21.png').
prototype_coordinates(if, 22, 0).
icon_size(if, 1, 1).


%  ===== ifvar ===========
section(ifvar, 'Control').
subsection(ifvar, 'Control').
description(ifvar, 'If the value of Va is true, O is A, else O is B. Var is some compile time variable (eg. DEV for development version).').
visual_style(ifvar,  normal).
inputs(ifvar, [required('Cond'), required('A'), required('B')]).
outputs(ifvar, ['O']).
input_types(ifvar, [string, object, object]).
output_types(ifvar, [object]).
image_name(ifvar, '/img/blocks/blocksheet0-_33.png').
prototype_coordinates(ifvar, 22, 1).
icon_size(ifvar, 1, 1).


%  ===== flipflop ===========
section(flipflop, 'Control').
subsection(flipflop, 'Control').
description(flipflop, 'a jk flipflop').
visual_style(flipflop,  normal).
inputs(flipflop, [required('J'), required('K')]).
outputs(flipflop, ['O']).
input_types(flipflop, [pulse, pulse]).
output_types(flipflop, [int]).
image_name(flipflop, '/img/blocks/blocksheet1_01.png').
prototype_coordinates(flipflop, 22, 2).
icon_size(flipflop, , ).



%  ===== isolate ===========
section(isolate, 'Editor Support').
subsection(isolate, 'Editor Support').
description(isolate, 'if the connection to A is visible, runs the network beyond A and passes A to O. Otherwise passes default to O. Used during development to prevent, for example, having to run the entire sound system because the sound system sends a hint to the CV system.').
visual_style(isolate,  normal).
inputs(isolate, [required('A'), required('Default')]).
outputs(isolate, ['O']).
input_types(isolate, [object, object]).
output_types(isolate, [object]).
image_name(isolate, '/img/blocks/blocksheet0-_18.png').
prototype_coordinates(isolate, 22, 3).
icon_size(isolate, 1, 1).


%  ===== inputparam ===========
section(inputparam, 'Editor Support').
subsection(inputparam, 'Editor Support').
description(inputparam, 'used when defining new blocks').
visual_style(inputparam,  visual_style0).
inputs(inputparam, [required('Name')]).
outputs(inputparam, ['O']).
input_types(inputparam, [string]).
output_types(inputparam, [object]).
image_name(inputparam, '/img/blocks/blocksheet0-_15.png').
prototype_coordinates(inputparam, 22, 4).
icon_size(inputparam, 1, 1).


%  ===== outputparam ===========
section(outputparam, 'Editor Support').
subsection(outputparam, 'Editor Support').
description(outputparam, 'used when defining new blocks').
visual_style(outputparam,  visual_style1).
inputs(outputparam, [required('A'), required('Name')]).
outputs(outputparam, []).
input_types(outputparam, [object, string]).
output_types(outputparam, []).
image_name(outputparam, '/img/blocks/blocksheet0-_29.png').
prototype_coordinates(outputparam, 22, 5).
icon_size(outputparam, 1, 1).



%  ===== func ===========
section(func, 'Math').
subsection(func, 'Math').
description(func, 'This special block has a textbox for a formula. So if you type 3x+2 you see an input parameter x. Operator meaning depends on whats connected to x (scalar or vector). Understands LSL angle bracket quat syntax').
visual_style(func,  normal).
inputs(func, [required('Formula'), required('A'), required('B')]).
outputs(func, ['O']).
input_types(func, [string, scalar, scalar, scalar]).
output_types(func, [object]).
image_name(func, '/img/blocks/blocksheet0-_32.png').
prototype_coordinates(func, 23, 0).
icon_size(func, 2, 1).



%  ===== delay ===========
section(delay, 'Misc').
subsection(delay, 'Misc').
description(delay, 'O is the previous stable value of A').
visual_style(delay,  normal).
inputs(delay, [required('A')]).
outputs(delay, ['O']).
input_types(delay, [object]).
output_types(delay, [object]).
image_name(delay, '/img/blocks/blocksheet0-_31.png').
prototype_coordinates(delay, 23, 1).
icon_size(delay, 1, 1).




%  ===== pulse ===========
section(pulse, 'I/O').
subsection(pulse, 'scalar').
description(pulse, 'Is 1 on the next frame after mouseup').
visual_style(pulse,  visual_style2).
inputs(pulse, []).
outputs(pulse, ['O']).
input_types(pulse, []).
output_types(pulse, [pulse]).
image_name(pulse, '/img/blocks/blocksheet1_01.png').
prototype_coordinates(pulse, 21, 0).
icon_size(pulse, 1, 1).


%  ===== proportion ===========
section(proportion, 'I/O').
subsection(proportion, 'scalar').
description(proportion, 'slider value from 0-1').
visual_style(proportion,  visual_style3).
inputs(proportion, []).
outputs(proportion, ['O']).
input_types(proportion, []).
output_types(proportion, [proportion]).
image_name(proportion, '/img/blocks/blocksheet1_02.png').
prototype_coordinates(proportion, 21, 1).
icon_size(proportion, 1, 1).


%  ===== floatval ===========
section(floatval, 'I/O').
subsection(floatval, 'scalar').
description(floatval, 'float value that varies over -inf to +inf').
visual_style(floatval,  visual_style4).
inputs(floatval, []).
outputs(floatval, ['O']).
input_types(floatval, []).
output_types(floatval, [float]).
image_name(floatval, '/img/blocks/blocksheet1_03.png').
prototype_coordinates(floatval, 21, 2).
icon_size(floatval, 1, 1).



%  ===== mic ===========
section(mic, 'I/O').
subsection(mic, 'sound').
description(mic, 'samples from mic').
visual_style(mic,  normal).
inputs(mic, []).
outputs(mic, ['O']).
input_types(mic, []).
output_types(mic, [sample]).
image_name(mic, '/img/blocks/blocksheet1_04.png').
prototype_coordinates(mic, 21, 3).
icon_size(mic, 1, 1).


%  ===== listen ===========
section(listen, 'I/O').
subsection(listen, 'sound').
description(listen, 'Sends sound to default output').
visual_style(listen,  visual_style5).
inputs(listen, [required('A')]).
outputs(listen, []).
input_types(listen, [sample]).
output_types(listen, []).
image_name(listen, '/img/blocks/blocksheet1_05.png').
prototype_coordinates(listen, 21, 4).
icon_size(listen, 1, 1).



%  ===== camera ===========
section(camera, 'I/O').
subsection(camera, 'imaging').
description(camera, 'O is frames out of camera, K is camera intrinsics').
visual_style(camera,  visual_style6).
inputs(camera, []).
outputs(camera, ['O', 'K']).
input_types(camera, []).
output_types(camera, [rgb, intrinsics]).
image_name(camera, '/img/blocks/blocksheet1_06.png').
prototype_coordinates(camera, 21, 5).
icon_size(camera, 1, 1).


%  ===== eyes ===========
section(eyes, 'I/O').
subsection(eyes, 'imaging').
description(eyes, 'input is commanded eye position, output is left and right eye cams').
visual_style(eyes,  normal).
inputs(eyes, [required('x'), required('y')]).
outputs(eyes, ['OL', 'OR']).
input_types(eyes, [proportion, proportion]).
output_types(eyes, [rgb, rgb]).
image_name(eyes, '/img/blocks/blocksheet1_07.png').
prototype_coordinates(eyes, 21, 6).
icon_size(eyes, 2, 1).


%  ===== videoplayer ===========
section(videoplayer, 'I/O').
subsection(videoplayer, 'imaging').
description(videoplayer, 'Sends frames out O, sound out S').
visual_style(videoplayer,  visual_style7).
inputs(videoplayer, []).
outputs(videoplayer, ['O', 'S']).
input_types(videoplayer, []).
output_types(videoplayer, [rgb, sample]).


%  ===== videorecord ===========
section(videorecord, 'I/O').
subsection(videorecord, 'imaging').
description(videorecord, 'Records frames to file. Figures out what to record from filetype and what\'s connected.').
visual_style(videorecord,  visual_style8).
inputs(videorecord, [optional('Vid'), optional('Snd')]).
outputs(videorecord, []).
input_types(videorecord, [image, sample]).
output_types(videorecord, []).


%  ===== spectrum ===========
section(spectrum, 'I/O').
subsection(spectrum, 'imaging').
description(spectrum, 'dancing spectrum display').
visual_style(spectrum,  visual_style9).
inputs(spectrum, [required('A')]).
outputs(spectrum, []).
input_types(spectrum, [sample]).
output_types(spectrum, []).
image_name(spectrum, '/img/blocks/blocksheet1_09.png').
prototype_coordinates(spectrum, 21, 7).
icon_size(spectrum, 1, 1).


%  ===== draw_tool ===========
section(draw_tool, 'I/O').
subsection(draw_tool, 'imaging').
description(draw_tool, 'output frame each time updated').
visual_style(draw_tool,  visual_style10).
inputs(draw_tool, []).
outputs(draw_tool, ['O']).
input_types(draw_tool, []).
output_types(draw_tool, [rgb]).


%  ===== viewer ===========
section(viewer, 'I/O').
subsection(viewer, 'imaging').
description(viewer, 'displays image').
visual_style(viewer,  visual_style11).
inputs(viewer, [required('A')]).
outputs(viewer, []).
input_types(viewer, [image]).
output_types(viewer, []).
image_name(viewer, '/img/blocks/blocksheet1_13.png').
prototype_coordinates(viewer, 21, 8).
icon_size(viewer, 1, 1).


%  ===== kernal ===========
section(kernal, 'I/O').
subsection(kernal, 'imaging').
description(kernal, 'kernal UI for defining kernals').
visual_style(kernal,  visual_style12).
inputs(kernal, []).
outputs(kernal, ['O']).
input_types(kernal, []).
output_types(kernal, [kernal]).
image_name(kernal, '/img/blocks/blocksheet1_14.png').
prototype_coordinates(kernal, 21, 9).
icon_size(kernal, 1, 1).



%  ===== bodymask ===========
section(bodymask, 'I/O').
subsection(bodymask, 'body').
description(bodymask, 'output is a vector of booleans for joints').
visual_style(bodymask,  visual_style13).
inputs(bodymask, []).
outputs(bodymask, ['O']).
input_types(bodymask, []).
output_types(bodymask, [mask]).
image_name(bodymask, '/img/blocks/blocksheet1_10.png').
prototype_coordinates(bodymask, 21, 10).
icon_size(bodymask, 1, 1).


%  ===== bodyposition ===========
section(bodyposition, 'I/O').
subsection(bodyposition, 'body').
description(bodyposition, 'output is a vector of joint positions from physivcal bot').
visual_style(bodyposition,  normal).
inputs(bodyposition, []).
outputs(bodyposition, ['O']).
input_types(bodyposition, []).
output_types(bodyposition, [joints]).
image_name(bodyposition, '/img/blocks/blocksheet1_11.png').
prototype_coordinates(bodyposition, 21, 11).
icon_size(bodyposition, 1, 1).


%  ===== bodypossim ===========
section(bodypossim, 'I/O').
subsection(bodypossim, 'body').
description(bodypossim, 'as per bodyposition but from the simulator').
visual_style(bodypossim,  normal).
inputs(bodypossim, []).
outputs(bodypossim, ['O']).
input_types(bodypossim, []).
output_types(bodypossim, [joints]).
image_name(bodypossim, '/img/blocks/blocksheet1_16.png').
prototype_coordinates(bodypossim, 21, 12).
icon_size(bodypossim, 1, 1).


%  ===== motors ===========
section(motors, 'I/O').
subsection(motors, 'body').
description(motors, 'Send channels of vector A matching Mask to robot servos').
visual_style(motors,  normal).
inputs(motors, [required('A'), required('Mask')]).
outputs(motors, []).
input_types(motors, [joints, jointmask]).
output_types(motors, []).
image_name(motors, '/img/blocks/blocksheet1_15.png').
prototype_coordinates(motors, 21, 13).
icon_size(motors, 1, 1).


%  ===== motorsim ===========
section(motorsim, 'I/O').
subsection(motorsim, 'body').
description(motorsim, 'As per motors, but to simulator').
visual_style(motorsim,  normal).
inputs(motorsim, [required('A'), required('Mask')]).
outputs(motorsim, []).
input_types(motorsim, [joints, jointmask]).
output_types(motorsim, []).
image_name(motorsim, '/img/blocks/blocksheet1_17.png').
prototype_coordinates(motorsim, 21, 14).
icon_size(motorsim, 1, 1).




%  ===== drbend ===========
section(drbend, 'connector').
subsection(drbend, 'pipe').
description(drbend, 'bidi pipe connection').
visual_style(drbend,  visual_style14).
inputs(drbend, [required('A')]).
outputs(drbend, ['O']).
input_types(drbend, [any]).
output_types(drbend, [any]).
image_name(drbend, '/img/blocks/blocksheet0-_02.png').
prototype_coordinates(drbend, 20, 0).
icon_size(drbend, 1, 1).


%  ===== dlbend ===========
section(dlbend, 'connector').
subsection(dlbend, 'pipe').
description(dlbend, 'bidi pipe connection').
visual_style(dlbend,  visual_style15).
inputs(dlbend, [required('A')]).
outputs(dlbend, ['O']).
input_types(dlbend, [any]).
output_types(dlbend, [any]).
image_name(dlbend, '/img/blocks/blocksheet0-_03.png').
prototype_coordinates(dlbend, 20, 1).
icon_size(dlbend, 1, 1).


%  ===== horpipe ===========
section(horpipe, 'connector').
subsection(horpipe, 'pipe').
description(horpipe, 'bidi pipe connection').
visual_style(horpipe,  visual_style16).
inputs(horpipe, [required('A')]).
outputs(horpipe, ['O']).
input_types(horpipe, [any]).
output_types(horpipe, [any]).
image_name(horpipe, '/img/blocks/blocksheet0-_04.png').
prototype_coordinates(horpipe, 20, 2).
icon_size(horpipe, 1, 1).


%  ===== cross ===========
section(cross, 'connector').
subsection(cross, 'pipe').
description(cross, 'bidi pipe connection').
visual_style(cross,  visual_style17).
inputs(cross, [required('A'), required('B')]).
outputs(cross, ['OA', 'OB']).
input_types(cross, [any, any]).
output_types(cross, [any, any]).
image_name(cross, '/img/blocks/blocksheet0-_05.png').
prototype_coordinates(cross, 20, 3).
icon_size(cross, 1, 1).


%  ===== urbend ===========
section(urbend, 'connector').
subsection(urbend, 'pipe').
description(urbend, 'bidi pipe connection').
visual_style(urbend,  visual_style18).
inputs(urbend, [required('A')]).
outputs(urbend, ['O']).
input_types(urbend, [any]).
output_types(urbend, [any]).
image_name(urbend, '/img/blocks/blocksheet0-_14.png').
prototype_coordinates(urbend, 20, 4).
icon_size(urbend, 1, 1).


%  ===== ulbend ===========
section(ulbend, 'connector').
subsection(ulbend, 'pipe').
description(ulbend, 'bidi pipe connection').
visual_style(ulbend,  visual_style19).
inputs(ulbend, [required('A')]).
outputs(ulbend, ['O']).
input_types(ulbend, [any]).
output_types(ulbend, [any]).
image_name(ulbend, '/img/blocks/blocksheet0-_11.png').
prototype_coordinates(ulbend, 20, 5).
icon_size(ulbend, 1, 1).


%  ===== distributor ===========
section(distributor, 'connector').
subsection(distributor, 'pipe').
description(distributor, 'bidi pipe connection').
visual_style(distributor,  visual_style20).
inputs(distributor, [required('A')]).
outputs(distributor, ['OA', 'OB', 'OC']).
input_types(distributor, [any]).
output_types(distributor, [any, any, any]).
image_name(distributor, '/img/blocks/blocksheet0-_09.png').
prototype_coordinates(distributor, 20, 6).
icon_size(distributor, 1, 1).


%  ===== vertpipe ===========
section(vertpipe, 'connector').
subsection(vertpipe, 'pipe').
description(vertpipe, 'bidi pipe connection').
visual_style(vertpipe,  visual_style21).
inputs(vertpipe, [required('A')]).
outputs(vertpipe, ['OA']).
input_types(vertpipe, [any]).
output_types(vertpipe, [any]).
image_name(vertpipe, '/img/blocks/blocksheet0-_08.png').
prototype_coordinates(vertpipe, 20, 7).
icon_size(vertpipe, 1, 1).




%  ===== black ===========
section(black, 'CV').
subsection(black, 'Image Processing').
description(black, 'outputs a solid black frame the camera size').
visual_style(black,  normal).
inputs(black, []).
outputs(black, ['O']).
input_types(black, []).
output_types(black, [mask]).


%  ===== white ===========
section(white, 'CV').
subsection(white, 'Image Processing').
description(white, 'outputs a solid white frame the camera size').
visual_style(white,  normal).
inputs(white, []).
outputs(white, ['O']).
input_types(white, []).
output_types(white, [mask]).


%  ===== threshold ===========
section(threshold, 'CV').
subsection(threshold, 'Image Processing').
description(threshold, 'out is mask with black in areas above threshold').
visual_style(threshold,  normal).
inputs(threshold, [required('A'), required('Thresh')]).
outputs(threshold, ['O']).
input_types(threshold, [image, proportion]).
output_types(threshold, [mask]).


%  ===== gray ===========
section(gray, 'CV').
subsection(gray, 'Image Processing').
description(gray, 'desaturates an image andoutputs grayscale image').
visual_style(gray,  normal).
inputs(gray, [required('A')]).
outputs(gray, ['O']).
input_types(gray, [image]).
output_types(gray, [gray]).


%  ===== hdr ===========
section(hdr, 'CV').
subsection(hdr, 'Image Processing').
description(hdr, 'converts image to high dynamic range from RGB or gray').
visual_style(hdr,  normal).
inputs(hdr, [required('A')]).
outputs(hdr, ['O']).
input_types(hdr, [image]).
output_types(hdr, [hdr]).


%  ===== dehdr ===========
section(dehdr, 'CV').
subsection(dehdr, 'Image Processing').
description(dehdr, 'converts HDR image to RGB').
visual_style(dehdr,  normal).
inputs(dehdr, [required('A')]).
outputs(dehdr, ['O']).
input_types(dehdr, [hdr]).
output_types(dehdr, [image]).


%  ===== channels ===========
section(channels, 'CV').
subsection(channels, 'Image Processing').
description(channels, 'takes 3 grayscale images and an optional mask or grayscale image and produces an RGB/RGBA image').
visual_style(channels,  normal).
inputs(channels, [required('R'), required('G'), required('B')]).
outputs(channels, ['O']).
input_types(channels, [gray, gray, gray, gray]).
output_types(channels, [image]).


%  ===== aschannels ===========
section(aschannels, 'CV').
subsection(aschannels, 'Image Processing').
description(aschannels, 'takes an RGB image and produces 3 grayscale images, one for each channel').
visual_style(aschannels,  normal).
inputs(aschannels, [required('A')]).
outputs(aschannels, ['R', 'G', 'B']).
input_types(aschannels, [rgb]).
output_types(aschannels, [gray, gray, gray]).


%  ===== pixel ===========
section(pixel, 'CV').
subsection(pixel, 'Image Processing').
description(pixel, 'as func, but has three formula boxes. Each one knows about r,g,b,m variables. Runs this formula for each pixel in image.').
visual_style(pixel,  visual_style22).
inputs(pixel, [required('A'), optional('M')]).
outputs(pixel, ['O']).
input_types(pixel, [image, mask]).
output_types(pixel, [image]).


%  ===== adaptivehistoequal ===========
section(adaptivehistoequal, 'CV').
subsection(adaptivehistoequal, 'Image Processing').
description(adaptivehistoequal, 'apply adaptive histogram equalization with blocksize Block and proportion Prop').
visual_style(adaptivehistoequal,  normal).
inputs(adaptivehistoequal, [required('A'), required('Prop'), required('Block')]).
outputs(adaptivehistoequal, ['O']).
input_types(adaptivehistoequal, [image, proportion, int]).
output_types(adaptivehistoequal, [image]).


%  ===== composite ===========
section(composite, 'CV').
subsection(composite, 'Image Processing').
description(composite, 'Output is A where Mask is white and B where it is black, and proportional mix in between').
visual_style(composite,  normal).
inputs(composite, [required('A'), required('B'), required('Mask')]).
outputs(composite, ['O']).
input_types(composite, [image, image, gray]).
output_types(composite, [image]).


%  ===== convolve ===========
section(convolve, 'CV').
subsection(convolve, 'Image Processing').
description(convolve, '').
visual_style(convolve,  normal).
inputs(convolve, [required('A'), required('Kernal')]).
outputs(convolve, ['O']).
input_types(convolve, [image, kernal]).
output_types(convolve, [image]).



%  ===== fft ===========
section(fft, 'CV').
subsection(fft, 'FrequencyDomain').
description(fft, 'Output is the forward fourier transform as complex16').
visual_style(fft,  normal).
inputs(fft, [required('A')]).
outputs(fft, ['O']).
input_types(fft, [image]).
output_types(fft, [complex16]).


%  ===== inv_fft ===========
section(inv_fft, 'CV').
subsection(inv_fft, 'FrequencyDomain').
description(inv_fft, 'Input is the ft spectrum as complex16, Output is the inverse fourier transform as gray').
visual_style(inv_fft,  normal).
inputs(inv_fft, [required('A')]).
outputs(inv_fft, ['O']).
input_types(inv_fft, [complex16]).
output_types(inv_fft, [gray]).



%  ===== imgadd ===========
section(imgadd, 'CV').
subsection(imgadd, 'combining').
description(imgadd, 'Ox = (Ax+Bx), for each channel x in the image (r,g,b, or grey)').
visual_style(imgadd,  normal).
inputs(imgadd, [required('A')]).
outputs(imgadd, ['O']).
input_types(imgadd, [image, image]).
output_types(imgadd, [image]).


%  ===== imgmax ===========
section(imgmax, 'CV').
subsection(imgmax, 'combining').
description(imgmax, 'takes the channel values from whichever image has R+G+B greatest').
visual_style(imgmax,  normal).
inputs(imgmax, [required('A')]).
outputs(imgmax, ['O']).
input_types(imgmax, [image, image]).
output_types(imgmax, [image]).















































