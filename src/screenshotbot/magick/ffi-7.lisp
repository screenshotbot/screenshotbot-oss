;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick/ffi-7
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                      #-lispworks
                      (#-lispworks #:fli #:util/fake-fli)                    )
  (:export
   #:alpha-channel-option
   #:exception-type
   #:composite-operator
   #:resource-type
   #:AreaResource
   #:DiskResource
   #:FileResource
   #:HeightResource
   #:MapResource
   #:MemoryResource
   #:ThreadResource
   #:ThrottleResource
   #:TimeResource
   #:WidthResource
   #:ListLengthResource
   #:OnAlphaChannel
   #:SrcCompositeOp
   #:SetAlphaChannel
   #:MeanSquaredErrorMetric
   #:RootMeanSquaredErrorMetric
   #:metric-type
   #:UndefinedException))
(in-package :screenshotbot/magick/ffi-7)

(fli:define-c-enum resource-type
  UndefinedResource
  AreaResource
  DiskResource
  FileResource
  HeightResource
  MapResource
  MemoryResource
  ThreadResource
  ThrottleResource
  TimeResource
  WidthResource
  ListLengthResource)

(fli:define-c-enum composite-operator
    UndefinedCompositeOp
  AlphaCompositeOp
  AtopCompositeOp
  BlendCompositeOp
  BlurCompositeOp
  BumpmapCompositeOp
  ChangeMaskCompositeOp
  ClearCompositeOp
  ColorBurnCompositeOp
  ColorDodgeCompositeOp
  ColorizeCompositeOp
  CopyBlackCompositeOp
  CopyBlueCompositeOp
  CopyCompositeOp
  CopyCyanCompositeOp
  CopyGreenCompositeOp
  CopyMagentaCompositeOp
  CopyAlphaCompositeOp
  CopyRedCompositeOp
  CopyYellowCompositeOp
  DarkenCompositeOp
  DarkenIntensityCompositeOp
  DifferenceCompositeOp
  DisplaceCompositeOp
  DissolveCompositeOp
  DistortCompositeOp
  DivideDstCompositeOp
  DivideSrcCompositeOp
  DstAtopCompositeOp
  DstCompositeOp
  DstInCompositeOp
  DstOutCompositeOp
  DstOverCompositeOp
  ExclusionCompositeOp
  HardLightCompositeOp
  HardMixCompositeOp
  HueCompositeOp
  InCompositeOp
  IntensityCompositeOp
  LightenCompositeOp
  LightenIntensityCompositeOp
  LinearBurnCompositeOp
  LinearDodgeCompositeOp
  LinearLightCompositeOp
  LuminizeCompositeOp
  MathematicsCompositeOp
  MinusDstCompositeOp
  MinusSrcCompositeOp
  ModulateCompositeOp
  ModulusAddCompositeOp
  ModulusSubtractCompositeOp
  MultiplyCompositeOp
  NoCompositeOp
  OutCompositeOp
  OverCompositeOp
  OverlayCompositeOp
  PegtopLightCompositeOp
  PinLightCompositeOp
  PlusCompositeOp
  ReplaceCompositeOp
  SaturateCompositeOp
  ScreenCompositeOp
  SoftLightCompositeOp
  SrcAtopCompositeOp
  SrcCompositeOp
  SrcInCompositeOp
  SrcOutCompositeOp
  SrcOverCompositeOp
  ThresholdCompositeOp
  VividLightCompositeOp
  XorCompositeOp
  StereoCompositeOp
  FreezeCompositeOp
  InterpolateCompositeOp
  NegateCompositeOp
  ReflectCompositeOp
  SoftBurnCompositeOp
  SoftDodgeCompositeOp
  StampCompositeOp
  RMSECompositeOp
  SaliencyBlendCompositeOp
  SeamlessBlendCompositeOp)

(fli:define-c-enum exception-type
    UndefinedException
  (  ResourceLimitWarning 300)
  (  WarningException 300)
  (  TypeWarning 305)
  (  OptionWarning 310)
  (  DelegateWarning 315)
  (  MissingDelegateWarning 320)
  (  CorruptImageWarning 325)
  (  FileOpenWarning 330)
  (  BlobWarning 335)
  (  StreamWarning 340)
  (  CacheWarning 345)
  (  CoderWarning 350)
  (  FilterWarning 352)
  (  ModuleWarning 355)
  (  DrawWarning 360)
  (  ImageWarning 365)
  (  WandWarning 370)
  (  RandomWarning 375)
  (  XServerWarning 380)
  (  MonitorWarning 385)
  (  RegistryWarning 390)
  (  ConfigureWarning 395)
  (  PolicyWarning 399)
  (  ErrorException 400)
  (  ResourceLimitError 400)
  (  TypeError 405)
  (  OptionError 410)
  (  DelegateError 415)
  (  MissingDelegateError 420)
  (  CorruptImageError 425)
  (  FileOpenError 430)
  (  BlobError 435)
  (  StreamError 440)
  (  CacheError 445)
  (  CoderError 450)
  (  FilterError 452)
  (  ModuleError 455)
  (  DrawError 460)
  (  ImageError 465)
  (  WandError 470)
  (  RandomError 475)
  (  XServerError 480)
  (  MonitorError 485)
  (  RegistryError 490)
  (  ConfigureError 495)
  (  PolicyError 499)
  (  FatalErrorException 700)
  (  ResourceLimitFatalError 700)
  (  TypeFatalError 705)
  (  OptionFatalError 710)
  (  DelegateFatalError 715)
  (  MissingDelegateFatalError 720)
  (  CorruptImageFatalError 725)
  (  FileOpenFatalError 730)
  (  BlobFatalError 735)
  (  StreamFatalError 740)
  (  CacheFatalError 745)
  (  CoderFatalError 750)
  (  FilterFatalError 752)
  (  ModuleFatalError 755)
  (  DrawFatalError 760)
  (  ImageFatalError 765)
  (  WandFatalError 770)
  (  RandomFatalError 775)
  (  XServerFatalError 780)
  (  MonitorFatalError 785)
  (  RegistryFatalError 790)
  (  ConfigureFatalError 795)
  (  PolicyFatalError 799 ))

(fli:define-c-enum alpha-channel-option
    UndefinedAlphaChannel
  ActivateAlphaChannel
  AssociateAlphaChannel
  BackgroundAlphaChannel
  CopyAlphaChannel
  DeactivateAlphaChannel
  DiscreteAlphaChannel
  DisassociateAlphaChannel
  ExtractAlphaChannel
  OffAlphaChannel
  OnAlphaChannel
  OpaqueAlphaChannel
  RemoveAlphaChannel
  SetAlphaChannel
  ShapeAlphaChannel
  TransparentAlphaChannel)

(fli:define-c-enum metric-type
    UndefinedErrorMetric
  AbsoluteErrorMetric
  FuzzErrorMetric
  MeanAbsoluteErrorMetric
  MeanErrorPerPixelErrorMetric
  MeanSquaredErrorMetric
  NormalizedCrossCorrelationErrorMetric
  PeakAbsoluteErrorMetric
  PeakSignalToNoiseRatioErrorMetric
  PerceptualHashErrorMetric
  RootMeanSquaredErrorMetric
  StructuralSimilarityErrorMetric
  StructuralDissimilarityErrorMetric)
