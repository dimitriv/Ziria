f = fopen('test_real_rx.infile', 'rb');
data = fread(f, inf, 'int16');
%data = load('test_rx.infile');
data=reshape(data, 2, length(data)/2);
data=data(1,:) + i*data(2,:);






d = load('test_real_rx.outfile');
df = d(1:2:end) + i*d(2:2:end);

%dt = df(139-80:end);
%dt = df(139:end);
dt = df;


% $$$ df = [];
% $$$ for j = 0:20
% $$$   df = [df, fft(dt((1:64)+(j)*80+10))];
% $$$ end
% $$$ 
% $$$ %figure; plot(X,abs(fft(dt((1:64)+s1*80+10))),X,abs(fft(dt((1:64)+s2*80+10))))
% $$$ 
% $$$ LTSf = [0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, ...
% $$$         1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, ...
% $$$         0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, ...
% $$$         1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1];
% $$$ LTSf = 2*LTSf-1;
% $$$ c = LTSf ./ df(1:64);
% $$$ ns = 16;
% $$$ dc = zeros(1, ns*64);
% $$$ for j = 0:ns-1
% $$$   dc((1:64)+j*64) = df((1:64)+(j+1)*64) .* c;
% $$$ end
% $$$ 
% $$$ ds = [];
% $$$ for j = 0:ns-1
% $$$   t = dc((1:64)+j*64);
% $$$   ds = [ds, t(38:38+5-1), t(44:44+13-1), t(58:58+6-1), t(1:6), t(8:8+13-1), t(22:22+5-1)];
% $$$ end


figure(1);
clf(1);

%XL = [1 1000];
XL = [1 12000];
subplot(3,1,1); plot(real(data)); title('Real(data)');
subplot(3,1,2); plot(abs(dt)); title('Abs of FFT');
%xlim(XL);
subplot(3,1,3); plot(angle(dt)); title('Phase of FFT');
%xlim(XL);


aaa


d = load('test_real_rx.outfile');
corr  = d(1:3:end);
corri = d(2:3:end);
ind   = d(3:3:end);

%XL = [0 10000];
XL = [8500 10000];

figure(1);
clf(1);
%Xr = (1:length(data)) - (1800*16);
Xr = (1:length(data));
subplot(4,1,1); plot(Xr, real(data/4)); title('Real(data)');
%xlim(XL);
X = (0:length(corr)-1)*16 + 1;
subplot(4,1,2); plot(X, corr); title('Corr'); 
%xlim(XL);
subplot(4,1,3); plot(X, corri); title('Number of consecutive increases'); 
%xlim(XL);
subplot(4,1,4); plot(X, ind); title('Index'); 
%xlim(XL);
