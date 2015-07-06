f = fopen('..\..\..\tests\test_real_rx.infile', 'rb');
data = fread(f, inf, 'int16');
data = data(1:2:end) + i*data(1:2:end);

%subsample
data = data(1:2:end);
start = 24762/2 ;
figure(1);
clf;
plot(real(data));
xlim([start start + 320]);
figure(2);
clf;
subplot(2,1,1);
X = 1:64;
plot(X, abs(fft(data(start+(1:64)))), X, abs(fft(data(start+64+(1:64)))));
subplot(2,1,2);
X = 1:12;
d1 = angle(fft(data(start+([5:4:25, 41:4:61]))));
d2 = angle(fft(data(start+64+([5:4:25, 41:4:61]))));
plot(X, d1, X, d2);
%plot(angle(fft(data(start+(1:64)))));
%plot(angle(fft(data(start+([5:4:25, 41:4:61])))));

figure(3);
clf;
start = 24762/2 + 170;
subplot(2,1,1);
X = 1:64;
plot(X, abs(fft(data(start+(1:64)))), X, abs(fft(data(start+64+(1:64)))));
subplot(2,1,2);
X = 1:12;
d1 = angle(fft(data(start+([5:4:25, 41:4:61]))));
d2 = angle(fft(data(start+64+([5:4:25, 41:4:61]))));
plot(X, d1, X, d2);
%plot(angle(fft(data(start+(1:64)))));
%plot(angle(fft(data(start+([5:4:25, 41:4:61])))));



nFFT = 64;
data = load('pkt3.txt');
data = data(1:2:end) + i*data(2:2:end);
figure(4);
clf;
start = 2;
plot(real(data));
xlim([start start + 320]);
figure(5);
clf;
start = start+1;
subplot(2,1,1);
X = 1:nFFT;
plot(X, abs(fft(data(start+(1:nFFT)))), X, abs(fft(data(start+nFFT+(1:nFFT)))));
subplot(2,1,2);
%plot(angle(fft(data(start+(1:nFFT)))));
X = 1:12;
d1 = angle(fft(data(start+([5:4:25, 41:4:61]))));
d2 = angle(fft(data(start+nFFT+([5:4:25, 41:4:61]))));
plot(X, d1, X, d2);

figure(6);
clf;
start = start + 170;
subplot(2,1,1);
X = 1:nFFT;
plot(X, abs(fft(data(start+(1:nFFT)))), X, abs(fft(data(start+nFFT+(1:nFFT)))));
subplot(2,1,2);
X = 1:12;
d1 = angle(fft(data(start+([5:4:25, 41:4:61]))));
d2 = angle(fft(data(start+nFFT+([5:4:25, 41:4:61]))));
plot(X, d1, X, d2);
%plot(angle(fft(data(start+(1:nFFT)))));
%plot(angle(fft(data(start+([5:4:25, 41:4:61])))));
