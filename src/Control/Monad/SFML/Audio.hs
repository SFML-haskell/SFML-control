{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.SFML.Audio where


import Control.Monad.SFML.Types.TH
import Control.Monad.SFML.Conversions
import qualified SFML.Audio as A

-- * Audio / Listener.hs
$(lift 'A.setGlobalVolume)
$(lift 'A.getGlobalVolume)
$(lift 'A.setListenerPosition)
$(lift 'A.getListenerPosition)
$(lift 'A.setListenerDirection)
$(lift 'A.getListenerDirection)

-- * Audio / Music.hs
$(liftWithDestroy 'err 'A.musicFromFile)
$(liftWithDestroy 'err 'A.musicFromMemory)
$(liftWithDestroy 'err 'A.musicFromStream)
$(lift 'A.setLoop)
$(lift 'A.getLoop)
$(lift 'A.getDuration)
$(lift 'A.play)
$(lift 'A.pause)
$(lift 'A.stop)
$(lift 'A.getChannelCount)
$(lift 'A.getSampleRate)
$(lift 'A.getStatus)
$(lift 'A.getPlayingOffset)
$(lift 'A.setPitch)
$(lift 'A.setVolume)
$(lift 'A.setPosition)
$(lift 'A.setRelativeToListener)
$(lift 'A.setMinDistance)
$(lift 'A.setAttenuation)
$(lift 'A.setPlayingOffset)
$(lift 'A.getPitch)
$(lift 'A.getVolume)
$(lift 'A.getPosition)
$(lift 'A.isRelativeToListener)
$(lift 'A.getMinDistance)
$(lift 'A.getAttenuation)

-- * Audio / Sound.hs
$(liftWithDestroy 'id 'A.createSound)
$(lift 'A.copySound)
$(lift 'A.setSoundBuffer)
$(lift 'A.getSoundBuffer)

-- * Audio / SoundBuffer.hs
$(liftWithDestroy 'err 'A.soundBufferFromFile)
$(liftWithDestroy 'err 'A.soundBufferFromMemory)
$(liftWithDestroy 'err 'A.soundBufferFromStream)
$(liftWithDestroy 'mb 'A.soundBufferFromSamples)
$(lift 'A.copySoundBuffer)
$(lift 'A.saveSoundBufferToFile)
$(lift 'A.getSamples)
$(lift 'A.getSampleCount)

-- * Audio / SoundBufferRecorder.hs
$(liftWithDestroy 'err 'A.createSoundBufferRecorder)
$(lift 'A.startRecording)
$(lift 'A.stopRecording)

-- * Audio / SoundRecorder.hs
$(liftWithDestroy 'err 'A.createSoundRecorder)
$(lift 'A.isSoundRecorderAvailable)

-- * Audio / SoundStream.hs
$(liftWithDestroy 'id 'A.createSoundStream)
