f = fopen('..\..\..\tests\test_real_rx.infile', 'rb');
data = fread(f, inf, 'int16');
data = data(1:2:end) + i*data(1:2:end);

%subsample
data = data(1:2:end);
start = 24762/2 ;
figure(2);
clf(2);
plot(real(data));
xlim([start start + 320]);
figure(1);
clf(1);
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

figure(5);
clf(5);
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




data = load('pkt1.txt');
data = data(1:2:end) + i*data(2:2:end);
figure(4);
clf(4);
start = 40;
plot(real(data));
xlim([start start + 320]);
figure(3);
clf(3);
start = 41;
subplot(2,1,1);
X = 1:64;
plot(X, abs(fft(data(start+(1:64)))), X, abs(fft(data(start+64+(1:64)))));
subplot(2,1,2);
%plot(angle(fft(data(start+(1:64)))));
X = 1:12;
d1 = angle(fft(data(start+([5:4:25, 41:4:61]))));
d2 = angle(fft(data(start+64+([5:4:25, 41:4:61]))));
plot(X, d1, X, d2);

figure(6);
clf(6);
start = 40 + 170;
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
