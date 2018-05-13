# conway-game-of-life
(yet another) implementation of the famous automaton, but this time done in Haskell on a RaspberryPi and displayed on a big 25x25 LED board.

Do note that this uses I2C for communicating between the Raspberry Pi and the Peggy2 board 
(for more details see https://trandi.wordpress.com/2016/09/09/electronic-art/ as it's the same hardware used)
for which the HPi (https://github.com/WJWH/HPi) Haskelly module is used, which in turn depends on the bcm2835 C library
being installed (http://airspayce.com/mikem/bcm2835/index.html).
