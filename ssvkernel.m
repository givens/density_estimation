function [y,t,optw,gs,C,confb95,yb] = ssvkernel(x,tin,option)
% [y,t,optw,W,C,confb95,yb] = sskernel(x,t,W)
%
% Function `sskernel' returns an optimized kernel density estimate 
% using a Gauss kernel function.
%
% Examples:
% >> x = 0.5-0.5*log(rand(1,1e3)); t = linspace(0,3,500);
% >> [y,t,optw] = ssvkernel(x,t);
% This example produces a vector of kernel density estimates, y, at points
% specified in a vector t, using locally adaptive bandwidths, optw 
% (a standard deviation of a normal density function).
% 
% >> ssvkernel(x);
% By calling the function without output arguments, the estimated density 
% is displayed.
%
% Input arguments:
% x:    Sample data vector. 
% tin (optinal):
%       Points at which estimation are computed. 
% W (optinal): 
%       A vector of kernel bandwidths. 
%       If W is provided, the optimal bandwidth is selected from the 
%       elements of W.
%       * Do not search bandwidths smaller than a sampling resolution of data.
%       If W is not provided, the program searches the optimal bandwidth
%       using a golden section search method. 
%
% Output arguments:
% y:    Estimated density
% t:    Points at which estimation was computed.
%       The same as tin if tin is provided. 
%       (If the sampling resolution of tin is smaller than the sampling 
%       resolution of the data, x, the estimation was done at smaller
%       number of points than t. The results, t and y, are obtained by 
%       interpolating the low resolution sampling points.)
% optw: Optimal kernel bandwidth.
% W:    Kernel bandwidths examined. 
% C:    Cost functions of W.
% conf95
%       Bootstrap confidence intervals.
% yb:   Booststrap samples.
%
% 
% Usage:
% >> [y,t,optw] = ssvkernel(x);
% When t is not given in the input arguments, i.e., the output argument t 
% is generated automatically.
%
% >> W = linspace(0.01,1,20);
% >> [y,t,optw] = ssvkernel(x,t,W);
% The optimal bandwidth is selected from the elements of W.
%
% >> [y,t,optw,confb95,yb] = ssvkernel(x);
% This additionally computes 95% bootstrap confidence intervals, confb95.
% The bootstrap samples are provided as yb.
% 
%
% Optimization principle:
% The optimization is based on a principle of minimizing 
% expected L2 loss function between the kernel estimate and an unknown 
% underlying density function. An assumption is merely that samples 
% are drawn from the density independently each other. 
%
% The locally adaptive bandwidth is obtained by iteratively computing
% optimal fixed-size bandwidths wihtihn local intervals. The optimal 
% bandwidths are selected such that they are selected in the intervals, 
% which are \gamma times larger than the optimal bandwidths themselves. 
% The paramter \gamma was optimized by minimizing the L2 risk estimate.
%
% The method is described in 
% Hideaki Shimazaki and Shigeru Shinomoto
% Kernel Bandwidth Optimization in Spike Rate Estimation 
% Journal of Computational Neuroscience 2010
% http://dx.doi.org/10.1007/s10827-009-0180-4
%
% * Instead of a Gaussian window described in the paper, a boxcar window is 
% used in this program.
%
% For more information, please visit 
% http://2000.jukuin.keio.ac.jp/shimazaki/res/kernel.html
%
% See also SSKERNEL, SSHIST
%
%
% This program is distributed under the terms of 
% the Creative Commons Attribution License (CC-BY)
% Hideaki Shimazaki 
% http://2000.jukuin.keio.ac.jp/shimazaki


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parameters Settings
M = 100;         %Number of bandwidths examined for optimization
                %The local optimal bandwidth is selcted from this. 

WinFunc = 'Boxcar'; %'Gauss','Laplace','Cauchy' 

x = reshape(x,1,numel(x));

if nargin == 1
    T = max(x) - min(x);
    [~,~,dt_samp] = find( sort(diff(sort(x))),1,'first');
    tin = linspace(min(x),max(x), min(ceil(T/dt_samp),1e3));
    t = tin;
    x_ab = x( logical((x >= min(tin)) .*(x <= max(tin))) ) ;
else
    T = max(tin) - min(tin);
    x_ab = x( logical((x >= min(tin)) .*(x <= max(tin))) ) ;
    [~,~,dt_samp] = find( sort(diff(sort(x_ab))),1,'first');

    if dt_samp > min(diff(tin))
        t = linspace(min(tin),max(tin), min(ceil(T/dt_samp),1e3));
    else
        t = tin;
    end
end

dt = min(diff(t));

% Compute a globally optimal fixed bandwidth
[yf,~,optWg] = sskernel(x,t);

% Create a finest histogram
y_hist = histc(x_ab,t-dt/2)/dt;
L = length(y_hist);
N = sum(y_hist*dt);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computing local MISEs and optimal bandwidths
disp('computing local bandwidths...');

logexp = @(x) log(1+exp(x)); 
ilogexp = @(x) log(exp(x)-1);

%Window sizes
%WIN = logexp(linspace(ilogexp(max(1*dt,optWg/10)),ilogexp(min(50*optWg,1*T)),M)); %%%
%WIN = logexp(linspace(ilogexp(max(1*dt,1/2*optWg)),ilogexp(1*T),M)); %Gauss best
WIN = logexp(linspace(ilogexp(max(1*dt)),ilogexp(1*T),M)); %Gauss best
W = WIN;        %Bandwidths

for i = M:-1:1
    Win = WIN(i);
    %W = logexp(linspace(ilogexp(max(1*dt)),ilogexp(Win),M));
    
    for j = 1: length(W)   
        w = W(j);
        yh = fftkernel(y_hist,w/dt);
        %yh = yh *N/sum(yh*dt);  %rate
        
        %computing local cost function
        c = yh.^2 - 2*yh.*y_hist + 2/sqrt(2*pi)/w*y_hist;
        C_local(j,:) = fftkernelWin(c,Win/dt,WinFunc); %Eq.15 for t= 1...L   
    end
    
    [~,n] = min(C_local,[],1); %find optw at t=1...L
    optws(i,:) = W(n);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Golden section search of the stiffness parameter of variable bandwidths.
% Selecting a bandwidth w/W = g. 

disp('adapting local bandwidths...');

% Initialization
tol = 10^-5; 
a = 1e-12; b = 1;
%a = 1.1; b = 1.11;

phi = (sqrt(5) + 1)/2;  %golden ratio

c1 = (phi-1)*a + (2-phi)*b;
c2 = (2-phi)*a + (phi-1)*b;

f1 = CostFunction(y_hist,N,t,dt,optws,WIN,WinFunc,c1);
f2 = CostFunction(y_hist,N,t,dt,optws,WIN,WinFunc,c2);
    
k = 1;
while  ( abs(b-a) > tol*(abs(c1)+abs(c2)) ) && k < 30
    if f1 < f2
        b = c2;
        c2 = c1;
        c1 = (phi - 1)*a + (2 - phi)*b;
        
        f2 = f1;
        [f1 yv1 optwp1] = CostFunction(y_hist,N,t,dt,optws,WIN,WinFunc,c1);
        
        %optg = c1;
        yopt = yv1 / sum(yv1*dt);
        optw = optwp1;
    else
        a = c1;
        c1 = c2;
        c2 = (2 - phi)*a + (phi - 1)*b;
        
        f1 = f2;
        [f2 yv2 optwp2] = CostFunction(y_hist,N,t,dt,optws,WIN,WinFunc,c2);
        
        %optg = c2;
        yopt = yv2 / sum(yv2*dt);
        optw = optwp2;
    end
    
    gs(k) = (c1);
    C(k) = f1;
    k = k + 1;
end
disp('optimization completed.');


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bootstrap Confidence Interval
if nargout == 0 || nargout >= 6 || nargin >= 3
    disp('computing bootstrap confidence intervals...');

    nbs = 1*1e2;        %number of bootstrap samples
    yb = zeros(nbs,L);

    for i = 1: nbs, %disp([i nbs])
        Nb = poissrnd(N);
        %Nb = N;
        idx = ceil(rand(1,Nb)*N);
        xb = x_ab(idx);
        y_histb = histc(xb,t-dt/2);
        
        idx = find(y_histb ~= 0);
        y_histb_nz = y_histb(idx); 
        t_nz = t(idx);
        for k = 1: L
            yb(i,k) = sum(y_histb_nz.*Gauss(t(k)-t_nz,optw(k)))/Nb;
        end
        yb(i,:) = yb(i,:) / sum(yb(i,:)*dt);
        
    end

    ybsort = sort(yb);
    y95b = ybsort(floor(0.05*nbs),:);
    y95u = ybsort(floor(0.95*nbs),:); 

    for i = 1: nbs
        yb(i,:) = interp1(t,yb(i,:),tin);
    end
    y95b = interp1(t,y95b,tin);
    y95u = interp1(t,y95u,tin);
    
    confb95 = [y95b; y95u];
    
    disp('done');
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Return results
y = interp1(t,yopt,tin);
optw = interp1(t,optw,tin);
t = tin;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Display results
if nargout == 0
            hold on;

            line([t; t],[y95b; y95u]...
                    ,'Color',[7 7 7]/8,'LineWidth',1 );
            plot(t,y95b,'Color',[7 7 7]/9,'LineWidth',1);
            plot(t,y95u,'Color',[7 7 7]/9,'LineWidth',1);

            plot(t,y,'Color',[0.9 0.2 0.2],'LineWidth',1);

            grid on;
            ylabel('density');
            set(gca,'TickDir','out');  
else
    if nargin >= 3
        if strcmp(option,'Visible')
            hold on; 
            
            if nargout >= 6
                line([t; t],[y95b; y95u]...
                    ,'Color',[7 7 7]/8,'LineWidth',1 );
                plot(t,y95b,'Color',[7 7 7]/9,'LineWidth',1);
                plot(t,y95u,'Color',[7 7 7]/9,'LineWidth',1);
            end

            plot(t,y,'Color',[0.9 0.2 0.2],'LineWidth',1);

            grid on;
            ylabel('density');
            set(gca,'TickDir','out'); 
            
        end
    end
    
end

function [Cg yv optwp] = CostFunction(y_hist,N,t,dt,optws,WIN,WinFunc,g)
%Selecting w/W = g bandwidth

L = length(y_hist);
optwv = zeros(1,L);
for k = 1: L
	gs = optws(:,k)'./WIN;
        
	if g > max( gs ) 
        optwv(k) = min(WIN);
    else
        if g < min(gs)
            optwv(k) = max(WIN);
        else
            idx = find(gs >= g, 1, 'last');
            optwv(k) = g*WIN(idx); 
            %optwv(k) = optws(idx,k);%exp
        end
    end
end


%Nadaraya-Watson kernel regression
PI = pi;
optwp = zeros(1,L);

if strcmp(WinFunc,'Laplace')
    for k = 1: L
        % Laplace window
        Lap = Laplace(t(k)-t,optwv/g); 
        optwp(k) = sum(optwv.*Lap)/sum(Lap);
    end
elseif strcmp(WinFunc,'Cauchy')
    for k = 1: L
        % Cauchy window
        Cau = Cauchy(t(k)-t,optwv/g); 
        optwp(k) = sum(optwv.*Cau)/sum(Cau);
    end  
elseif strcmp(WinFunc,'Boxcar')
    for k = 1: L
        % Boxcar window
        Box = Boxcar(t(k)-t,optwv/g); 
        optwp(k) = sum(optwv.*Box)/sum(Box);
    end  
else
    for k = 1: L
        % Gauss window
        G = Gauss(t(k)-t,optwv/g); 
        optwp(k) = sum(optwv.*G)/sum(G); 
    end
end
%optwp = optwv;
%Density estimation with the variable bandwidth

% Baloon estimator
%yv = zeros(1,L);
%for k = 1: L
%    yv(k) = sum( y_hist*dt.*Gauss(t(k)-t,optwp(k),PI) ) / N;    
%end
%yv = yv / sum(yv*dt);

% Baloon estimator (speed optimized)
idx = find(y_hist ~= 0);
y_hist_nz = y_hist(idx); 
t_nz = t(idx);

yv = zeros(1,L);
for k = 1: L
    yv(k) = sum( y_hist_nz*dt.*Gauss(t(k)-t_nz,optwp(k)));    
end
yv = yv *N/sum(yv*dt); %rate

% Sample points estimator
%yv = zeros(1,L);
%for k = 1: L
%    yv(k) = sum( y_hist_nz*dt.*Gauss(t(k)-t_nz,optwp(idx),PI) ) / N;    
%end
%yv = yv / sum(yv*dt);

% Kernel regression
if 0
for k = 1: L
	yv(k) = sum(y_hist.*Gauss(t(k)-t,optwp,PI))...
        /sum(Gauss(t(k)-t,optwp,PI));
end
yv = yv *N/ sum(yv*dt);
end

%Cost function of the estimated density
cg = yv.^2 - 2*yv.*y_hist + 2/sqrt(2*pi)./optwp.*y_hist;
Cg = sum(cg*dt);


function [y] = fftkernel(x,w)
L = length(x);
Lmax = L+3*w; %take 3 sigma to avoid aliasing 

%n = 2^(nextpow2(Lmax)); 
n = 2^(ceil(log2(Lmax))); 

X = fft(x,n);

f = [-(0:n/2) (n/2-1:-1:1)]/n;

% Gauss
K = exp(-0.5*(w*2*pi*f).^2);
%K = 1 ./ ( 1+ (w*2*pi*f).^2/2 );

y = ifft(X.*K,n);
y = y(1:L);

function [y] = fftkernelWin(x,w,WinFunc)
L = length(x);
Lmax = L+3*w; %take 3 sigma to avoid aliasing 

%n = 2^(nextpow2(Lmax)); 
n = 2^(ceil(log2(Lmax))); 

X = fft(x,n);

f = [-(0:n/2) (n/2-1:-1:1)]/n;

if strcmp(WinFunc,'Laplace')
    % Laplace
    K = 1 ./ ( 1+ (w*2*pi.*f).^2/2 );
elseif strcmp(WinFunc,'Cauchy')
    % Cauchy
    K = exp(-w*abs(2*pi*f));
elseif strcmp(WinFunc,'Boxcar')
    % Boxcar
    a = sqrt(12)*w;
    %K = (exp(1i*2*pi*f*a/2) - exp(-1i*2*pi*f*a/2)) ./(1i*2*pi*f*a);
    K = 2./(a* 2*pi*f) .* sin(a/2.* 2*pi*f);
    K(1) = 1;
else
    % Gauss
    K = exp(-0.5*(w*2*pi*f).^2);
end

y = ifft(X.*K,n);
y = y(1:L);

function y = Gauss(x,w) 
y = 1/sqrt(2*pi)./w.*exp(-x.^2/2./w.^2);

function y = Laplace(x,w)
y = 1./sqrt(2)./w.*exp(-sqrt(2)./w.*abs(x));

function y = Cauchy(x,w) 
y = 1./(pi*w.*(1+ (x./w).^2));

function y = Boxcar(x,w)
a = sqrt(12)*w;
y = 1./a .* ( x < a/2 ) .* ( x > -a/2 );