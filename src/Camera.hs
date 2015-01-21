module Camera where

import Data.Vec
import qualified Data.Vec as V
import qualified AbstractCamera as C

data ViewTrafo = ViewTrafo { 
  right :: Vec3F, up :: Vec3F, forward :: Vec3F,
  trafo :: C.Trafo,
  position :: Vec3F
}

lookAt ::  Vec3F -> Vec3F -> Vec3F -> ViewTrafo
lookAt eyeVec lookAtVec upVec = ViewTrafo  { right = side', up = up', forward = forward',
                                             trafo = trafo', position = eyeVec }
  where up'      = normalize $ side'    `cross'` forward'
        side'    = normalize $ forward' `cross'` upVec
        forward' = normalize $ lookAtVec - eyeVec
        mat = blowUp  side' 0       :. 
              blowUp  up'   0       :. 
              blowUp (-forward') 0  :. 
              nullPnt               :. ()
        t = translation (unpack $ -eyeVec)
        trafo' = C.Trafo3D { C.forward =  mat `multmm` t, 
                            C.backward = undefined }

changeByForward :: Vec3F -> ViewTrafo -> ViewTrafo
changeByForward forward s = ViewTrafo { right = side', up = up', forward = forward',
                                        trafo = trafo', position = position' }
  where position' = position s
        side' = normalize $ forward' `cross'` (pack $ V.take n3 y)
        up'   = normalize $ side' `cross'` forward'
        forward' = normalize forward
        mat = blowUp side' 0 :.
              blowUp up' 0 :.
              blowUp (-forward) 0 :.
              nullPnt :.()
        t = translation (unpack $ -position')
        trafo' = C.Trafo3D { C.forward = mat `multmm` t, 
                             C.backward = undefined }

moveForward :: Float -> ViewTrafo -> ViewTrafo
moveForward d view = view { position = (position view) + pack ((unpack $ forward view) `smul` d) }

strafeView :: Float -> ViewTrafo -> ViewTrafo
strafeView d view = view { position = (position view) + pack ((unpack $ right view) `smul` d) }

smul :: Vec3 Float -> Float -> Vec3 Float
smul (x:.y:.z:.()) s = (x*s):.(y*s):.(z*s):.()

cross' :: Vec3F -> Vec3F -> Vec3F 
cross' a b = pack $ unpack a `cross` unpack b

nullPnt :: Vec4 Float
nullPnt = 0:.0:.0:.1:.()

x,y,z :: Vec4 Float
x = 1 :. 0 :. 0 :. 0 :. () :: Vec4 Float
y = 0 :. 1 :. 0 :. 0 :. () :: Vec4 Float
z = 0 :. 0 :. 1 :. 0 :. () :: Vec4 Float

blowUp :: Vec3F -> Float -> Vec4 Float
blowUp xs val = unpack xs `snoc` val
