f = fopen('debug_BPSK_12.bin', 'rb');
data = fread(f, inf, 'int16');
%data = load('rx.infile');
data=data(1:floor(end/2)*2);
data=reshape(data, 2, length(data)/2);
data=data(1,:) + i*data(2,:);


PLOT_OUT = 1;

if PLOT_OUT
  d = load('rx.outfile');
  dt = d(1:2:end) + i*d(2:2:end);
  
  figure(1);
  clf(1);

  %XL = [1 1000];
  XL = [1 12000];
  subplot(3,1,1); plot(real(data)); title('Real(data)');
  %subplot(3,1,2); plot(abs(dt)); title('Abs of FFT');
  subplot(3,1,2); plot(real(dt)); title('Abs of FFT');
  xlim(XL);
  subplot(3,1,3); plot(angle(dt)); title('Phase of FFT');
  xlim(XL);

else

  d = load('rx.outfile');
  corr  = d(1:3:end);
  corri = d(2:3:end);
  ind   = d(3:3:end);

  %XL = [0 10000];
  %XL = [8500 10000];
  XL = [0 15700] + 25000;

  figure(1);
  clf(1);
  %Xr = (1:length(data)) - (1800*16);
  Xr = (1:length(data));
  subplot(4,1,1); plot(Xr, real(data/4)); title('Real(data)');
  %xlim(XL);
  %X = (0:length(corr)-1)*16 + 1;
  X = (0:length(corr)-1) + 1;
  subplot(4,1,2); plot(X, corr); title('Corr'); 
  %xlim(XL);
  subplot(4,1,3); plot(X, corri); title('Number of consecutive increases'); 
  %xlim(XL);
  subplot(4,1,4); plot(X, ind); title('Index'); 
  %xlim(XL);
end
