;;;; Copyright 2018-Present Modern Interpreters Inc.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(defpackage :screenshotbot/magick/ffi-6
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
   #:ResourceLimitWarning
   #:UndefinedException
   #:RootMeanSquaredErrorMetric
   #:metric-type))
(in-package :screenshotbot/magick/ffi-6)

(fli:define-c-enum resource-type
  UndefinedResource
  AreaResource
  DiskResource
  FileResource
  MapResource
  MemoryResource
  ThreadResource
  TimeResource
  ThrottleResource
  WidthResource
  HeightResource
  ListLengthResource)

(fli:define-c-enum composite-operator
  UndefinedCompositeOp
  NoCompositeOp
  ModulusAddCompositeOp
  AtopCompositeOp
  BlendCompositeOp
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
  CopyOpacityCompositeOp
  CopyRedCompositeOp
  CopyYellowCompositeOp
  DarkenCompositeOp
  DstAtopCompositeOp
  DstCompositeOp
  DstInCompositeOp
  DstOutCompositeOp
  DstOverCompositeOp
  DifferenceCompositeOp
  DisplaceCompositeOp
  DissolveCompositeOp
  ExclusionCompositeOp
  HardLightCompositeOp
  HueCompositeOp
  InCompositeOp
  LightenCompositeOp
  LinearLightCompositeOp
  LuminizeCompositeOp
  MinusDstCompositeOp
  ModulateCompositeOp
  MultiplyCompositeOp
  OutCompositeOp
  OverCompositeOp
  OverlayCompositeOp
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
  ModulusSubtractCompositeOp
  ThresholdCompositeOp
  XorCompositeOp
  DivideDstCompositeOp
  DistortCompositeOp
  BlurCompositeOp
  PegtopLightCompositeOp
  VividLightCompositeOp
  PinLightCompositeOp
  LinearDodgeCompositeOp
  LinearBurnCompositeOp
  MathematicsCompositeOp
  DivideSrcCompositeOp
  MinusSrcCompositeOp
  DarkenIntensityCompositeOp
  LightenIntensityCompositeOp
  HardMixCompositeOp
  StereoCompositeOp)

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
  BackgroundAlphaChannel
  CopyAlphaChannel
  DeactivateAlphaChannel
  ExtractAlphaChannel
  OpaqueAlphaChannel
  ResetAlphaChannel
  SetAlphaChannel
  ShapeAlphaChannel
  TransparentAlphaChannel
  FlattenAlphaChannel
  RemoveAlphaChannel
  AssociateAlphaChannel
  DisassociateAlphaChannel)

(fli:define-c-enum metric-type
  UndefinedMetric
  AbsoluteErrorMetric
  MeanAbsoluteErrorMetric
  MeanErrorPerPixelMetric
  MeanSquaredErrorMetric
  PeakAbsoluteErrorMetric
  PeakSignalToNoiseRatioMetric
  RootMeanSquaredErrorMetric
  NormalizedCrossCorrelationErrorMetric
  FuzzErrorMetric
  (UndefinedErrorMetric 0)
  (PerceptualHashErrorMetric #xff))
