f = fopen('test_real_rx.infile', 'rb');
data = fread(f, inf, 'int16');
%data = load('test_rx.infile');
data=reshape(data, 2, length(data)/2);
data=data(1,:) + i*data(2,:);






d = load('test_real_rx.outfile');
df = d(1:2:end) + i*d(2:2:end);

LTSf = [0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, ...
        1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, ...
        0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, ...
        1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1];
LTSf = 2*LTSf-1;


figure(1);
clf(1);
XL = [1 1000];
subplot(3,1,1); plot(real(data/4)); title('Real(data)');
subplot(3,1,2); plot(abs(df)); title('Abs of FFT');
xlim(XL);
subplot(3,1,3); plot(angle(df)); title('Phase of FFT');
xlim(XL);




aaa


d = load('test_real_rx.outfile');
corr  = d(1:3:end);
corri = d(2:3:end);
ind   = d(3:3:end);

XL = [0 1600];
XL = [400 800];

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
