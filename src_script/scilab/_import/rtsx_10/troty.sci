// TROTY.SCI  basic homogeneous transformation for rotation about Y axis
// www.controlsystemslab.com  July 2012
//
// Example:
// T = troty(%pi/2);

function T=troty(theta, varargin)
    opt.deg = 0;
    // process varargin 
    numopts=length(varargin); // find number of options
    if numopts>0 then
        for i=1:numopts
            if varargin(i)== 'deg' then opt.deg = 1;
            end
        end
    end
    // optionally convert from degrees
    if opt.deg then
        d2r = %pi/180.0;
        theta = d2r*theta;
    end    
    cth=cos(theta);
    sth=sin(theta);
    T=clean([cth 0 sth 0;0 1 0 0;-sth 0 cth 0; 0 0 0 1]);
endfunction