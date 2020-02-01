# hsoapysdr
Very simple Haskell wrapper around SoapySDR library.

I liked the idea of SDR library by Adam Walker (https://github.com/adamwalker/sdr), but wanted support for other hardware types like HackRF and LimeSDR Mini, so I put together this crude stuff.

# Screenshot
A chunk of the 433 MHz ISM spectrum. Captured with an RTLSDR device and drawn as a waterfall using the [Plot](https://github.com/adamwalker/sdr/blob/master/hs_sources/SDR/Plot.hs) module.

![Screenshot](https://github.com/kmikeru/hsoapysdr/blob/master/8f9db17a-837a-4875-aa89-e03117382e20.jpeg?raw=true)

Only tested on Debian x86-64 with bundled GHC 8.6.5.

PRs are welcome.

I still have no idea what I'm doing. (c)
