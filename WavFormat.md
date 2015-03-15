# Introduction #

wAviCLe uses a certain subset of the WAV format which is uncompressed. It is documented below.

More information:
http://ccrma.stanford.edu/courses/422/projects/WaveFormat/

# Details #

Our parsing of the WAV file header appears to be accurate. What we read from a file matches what audio-editing programs read. The sound data appears to be stored in one of four ways, and needs to be parsed differently depending on the number of bits per sample and the number of audio channels (stereo or mono).

For the description, we will read 4 bytes from the data portion of the WAV file: b1 b2 b3 b4

For 8bps mono files, that would be four separate samples, each with values ranging from 0 to 255.

For 8bps stereo files, we would have two samples: (b1 b2) (b3 b4). In this case, b1 and b3 are successive left-channel samples and b2 and b4 are successive right-channel samples.

For 16bps mono files (our test file, click.wav, is an example of this), b1 and b2 are combined into a single sample. They are read concurrently as a 16-bit number and interpreted as a 2's complement value. b3 and b4 are treated the same way so the four bytes represent 2 samples on the same channel.

For 16bps stereo files, the four bytes would be read as a single sample. Like the other 16-bit example, the pairs of bytes need to be combined and interpreted as 2's complement, but the resulting values are left and right channel samples.