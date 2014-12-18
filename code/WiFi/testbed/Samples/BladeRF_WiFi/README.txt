The packets in this directory are WiFi captures from my home router obtained by Blade RF. 
Packets pkt?.txt looks like correct packets, though some of them have the STS preamble of 80 samples,
followed by something unexpected (as opposed to 160+ samples of STS in 802.11a/g), so
they are possibly 802.11n packets, though I haven't checked. 

Packets beacon_pkt?.txt lookt like beacon packets, transmitted every 1s, but they have a weird shape, 
which is most likely caused by a frequency offset. I suspect that packets above come from my mobile phone
(as they are taken while a mobile download was ongoing), and packets here come from the AP itself, thus
with a different clock. 

