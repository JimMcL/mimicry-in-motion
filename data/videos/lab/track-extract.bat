@echo off

rem Parameters were defined individually for videos, or batches of
rem similar videos, so this file serves more as a record of extraction
rem parameters than a script to perform all extractions.

set TR=cmd /c C:\Jim\uni\apps\YetAnotherTracker\YetAnotherTracker.bat %*

rem %TR% --default tracker.defaults --view-scale ? 2471.mp4
rem %TR% --default tracker.defaults --view-scale ? 2472.mp4
rem %TR% --default tracker.defaults --view-scale ? 2473.mp4
rem %TR% --default tracker.defaults --view-scale ? 2474.mp4
rem %TR% --default high-contrast.def --view-scale 2040.03 --csv-units m 3493.mp4
rem %TR% --default high-contrast.def --view-scale 2063.89 --csv-units m 3494.mp4
rem %TR% --default high-contrast.def --view-scale 2068.35 --csv-units m 3495.mp4
rem %TR% --default high-contrast.def --view-scale 2061.76 --csv-units m --min-contour 20 --max-contour 600 --max-jump 100 --threshold 127 --first-tracking-frame 1 3496.mp4
rem %TR% --default high-contrast.def --view-scale 2066.21 --csv-units m 3497.mp4
rem %TR% --default high-contrast.def --view-scale 2068.62 --csv-units m 3498.mp4
rem %TR% --default high-contrast.def --view-scale 2070.62 --csv-units m --max-jump 50 3499.mp4
rem %TR% --default high-contrast.def --view-scale 2022.61 --csv-units m 3500.mp4
rem %TR% --default high-contrast.def --view-scale 2022.84 --csv-units m 3501.mp4
rem %TR% --default high-contrast.def --view-scale 3155.55 --csv-units m 3502.mp4
rem %TR% --default high-contrast.def --view-scale 3146.77 --csv-units m 3503.mp4
rem %TR% --default high-contrast.def --view-scale 2057.56 --csv-units m 3504.mp4
rem %TR% --default high-contrast.def --view-scale 2066.13 --csv-units m 3505.mp4
rem %TR% --default high-contrast.def --view-scale 2068.76 --csv-units m 3506.mp4
rem %TR% --default high-contrast.def --view-scale 2032.14 --csv-units m 3507.mp4
rem %TR% --default high-contrast.def --view-scale 2062.06 --csv-units m 3508.mp4
rem %TR% --default high-contrast.def --view-scale 2068.99 --csv-units m 3509.mp4
rem %TR% --default high-contrast.def --view-scale 2064.47 --csv-units m 3510.mp4
rem %TR% --default high-contrast.def --view-scale 2061.90 --csv-units m 3511.mp4
rem %TR% --default high-contrast.def --view-scale 2059.88 --csv-units m 3512.mp4
rem %TR% --default high-contrast.def --view-scale 2055.58 --csv-units m 3513.mp4
rem %TR% --default high-contrast.def --view-scale 2064.65 --csv-units m 3514.mp4
rem %TR% --default high-contrast.def --view-scale 2068.62 --csv-units m --min-contour 10 --max-contour 100 3515.mp4
rem %TR% --default high-contrast.def --view-scale 2025.62 --csv-units m 3516.mp4
rem %TR% --default high-contrast.def --view-scale 2064.11 --csv-units m 3517.mp4
rem %TR% --default high-contrast.def --view-scale 2068.93 --csv-units m --min-contour 10 --max-contour 100 3518.mp4
rem %TR% --default high-contrast.def --view-scale 2024.88 --csv-units m --max-jump 50 3519.mp4
rem %TR% --default high-contrast.def --view-scale 3375.70 --csv-units m --min-contour 10 --max-contour 100 --threshold 90 3520.mp4
rem %TR% --default adaptive.def --view-scale 3355.98 --csv-units m --min-contour 10 --max-contour 100 3521.mp4
rem %TR% --default high-contrast.def --view-scale 2077.11 --csv-units m 3522.mp4
rem %TR% --default high-contrast.def --view-scale 1370.49 --csv-units m 3523.mp4
rem %TR% --default high-contrast.def --view-scale 2070.79 --csv-units m 3524.mp4
rem %TR% --default high-contrast.def --view-scale 2064.15 --csv-units m 3525.mp4
rem %TR% --default high-contrast.def --view-scale 2081.36 --csv-units m 3526.mp4
rem %TR% --default high-contrast.def --view-scale 3484.81 --csv-units m 3527.mp4
rem %TR% --default high-contrast.def --view-scale 3226.24 --csv-units m 3528.mp4
rem %TR% --default high-contrast.def --view-scale 3234.53 --csv-units m 3529.mp4
rem %TR% --default high-contrast.def --view-scale 3225.85 --csv-units m 3530.mp4
rem %TR% --default high-contrast.def --view-scale 3260.90 --csv-units m --max-contour 4000 --mask-file 3531.json 3531.mp4
rem %TR% --default high-contrast.def --view-scale 3225.75 --csv-units m 3532.mp4
rem %TR% --default high-contrast.def --view-scale 3234.71 --csv-units m --mask-file 3533.json 3533.mp4
rem %TR% --default high-contrast.def --view-scale 3247.71 --csv-units m 3534.mp4
rem %TR% --default high-contrast.def --view-scale 3225.74 --csv-units m 3535.mp4
rem %TR% --default high-contrast.def --view-scale 3265.27 --csv-units m 3536.mp4
rem %TR% --default high-contrast.def --view-scale 3217.07 --csv-units m 3537.mp4
rem %TR% --default high-contrast.def --view-scale 3230.14 --csv-units m --mask-file 3538.json 3538.mp4
rem %TR% --default high-contrast.def --view-scale 3234.71 --csv-units m 3539.mp4
rem %TR% --default high-contrast.def --view-scale 3243.30 --csv-units m 3540.mp4
rem %TR% --default high-contrast.def --view-scale 3238.92 --csv-units m 3541.mp4
rem %TR% --default high-contrast.def --view-scale 3165.40 --csv-units m 3542.mp4
rem %TR% --default high-contrast.def --view-scale 3218.83 --csv-units m --dilation-erosion 10 3543.mp4
rem %TR% --default high-contrast.def --view-scale 3139.88 --csv-units m --min-contour 10 --max-contour 200 --threshold 70 3544.mp4
rem %TR% --default high-contrast.def --view-scale 3190.66 --csv-units m 3545.mp4
rem %TR% --default high-contrast.def --view-scale 3173.51 --csv-units m --min-contour 10 --max-contour 200 3546.mp4
rem %TR% --default high-contrast.def --view-scale 3216.99 --csv-units m 3547.mp4
rem %TR% --default high-contrast.def --view-scale 3195.09 --csv-units m --min-contour 20 --max-contour 300 --threshold 120 --dilation-erosion 10 --mask-file 3548.json 3548.mp4
rem %TR% --default high-contrast.def --view-scale 2077.07 --csv-units m 3568.mp4
rem %TR% --default high-contrast.def --view-scale 2066.36 --csv-units m 3569.mp4
rem %TR% --default high-contrast.def --view-scale 2059.48 --csv-units m 3570.mp4
rem %TR% --default high-contrast.def --view-scale 2052.98 --csv-units m 3571.mp4
rem %TR% --default high-contrast.def --view-scale 2050.98 --csv-units m 3573.mp4
rem %TR% --default high-contrast.def --view-scale 1933.53 --csv-units m --min-contour 1 --first-tracking-frame 1 --max-jump 50 3574.mp4
rem %TR% --default high-contrast.def --view-scale 1927.16 --csv-units m --min-contour 10 --max-contour 100 3575.mp4
rem %TR% --default high-contrast.def --view-scale 1996.90 --csv-units m 3576.mp4
rem %TR% --default high-contrast.def --view-scale 1920.64 --csv-units m 3578.mp4
rem %TR% --default high-contrast.def --view-scale 1933.48 --csv-units m 3579.mp4
rem %TR% --default high-contrast.def --view-scale 1966.09 --csv-units m 3580.mp4
rem %TR% --default high-contrast.def --view-scale 1979.23 --csv-units m --min-contour 10 --max-contour 100 3581.mp4
rem %TR% --default high-contrast.def --view-scale 1993.49 --csv-units m --min-contour 10 --max-contour 200 3582.mp4
rem %TR% --default high-contrast.def --view-scale 2005.30 --csv-units m --min-contour 10 --max-contour 200 3583.mp4
rem %TR% --default high-contrast.def --view-scale 1923.37 --csv-units m --min-contour 20 3584.mp4
rem %TR% --default high-contrast.def --view-scale 2005.58 --csv-units m --min-contour 10 3585.mp4
rem %TR% --default high-contrast.def --view-scale 2005.33 --csv-units m --min-contour 10 3586.mp4
rem %TR% --default high-contrast.def --view-scale 1935.67 --csv-units m --min-contour 20 3606.mp4
rem %TR% --default high-contrast.def --view-scale 1903.33 --csv-units m --min-contour 10 3608.mp4
rem %TR% --default high-contrast.def --view-scale 1916.09 --csv-units m --min-contour 20 3610.mp4
rem %TR% --default high-contrast.def --view-scale 1926.97 --csv-units m --min-contour 20 3614.mp4
rem %TR% --default high-contrast.def --view-scale 1907.42 --csv-units m --min-contour 20 3615.mp4
rem %TR% --default high-contrast.def --view-scale 1896.55 --csv-units m --min-contour 10 3617.mp4
rem %TR% --default high-contrast.def --view-scale 1926.95 --csv-units m --min-contour 10 3621.mp4
rem %TR% --default high-contrast.def --view-scale 1907.48 --csv-units m --min-contour 20 3625.mp4
rem %TR% --default high-contrast.def --view-scale 1907.42 --csv-units m --min-contour 20 3627.mp4
rem %TR% --default high-contrast.def --view-scale 1905.23 --csv-units m --first-tracking-frame 10 --min-contour 20 3630.mp4
rem %TR% --default high-contrast.def --view-scale 1900.88 --csv-units m --min-contour 20 --mask-file 3633.json 3633.mp4
rem %TR% --default high-contrast.def --view-scale 1900.89 --csv-units m --first-tracking-frame 10 --min-contour 20 3637.mp4
rem %TR% --default high-contrast.def --view-scale 1911.79 --csv-units m --first-tracking-frame 10 --min-contour 20 3639.mp4
rem %TR% --default high-contrast.def --view-scale 1905.24 --csv-units m --first-tracking-frame 10 --min-contour 20 3642.mp4
rem %TR% --default high-contrast-small.def --view-scale 1896.56 --csv-units m 3645.mp4
rem %TR% --default high-contrast-small.def --view-scale 1914.27 --csv-units m 3648.mp4
rem %TR% --default high-contrast-small.def --view-scale 1903.07 --csv-units m 3651.mp4
rem %TR% --default high-contrast-small.def --view-scale 1913.96 --csv-units m 3652.mp4
rem %TR% --default high-contrast-small.def --view-scale 2059.50 --csv-units m 3654.mp4
rem %TR% --default high-contrast-small.def --fps 240 --view-rotation -90 --view-scale 1189.40 --csv-units m --mask-file 4325.json --threshold 50 4325.mp4
rem %TR% --default high-contrast-small.def --fps 240 --view-rotation -90 --view-scale 1189.40 --csv-units m --mask-file 4326.json 4326.mp4
rem %TR% --default high-contrast-small.def --fps 240 --view-rotation -90 --view-scale 1189.12 --csv-units m --threshold 50 --mask-file 4327.json 4327.mp4
rem %TR% --default high-contrast-small.def --fps 240 --view-rotation -90 --view-scale 1185.13 --csv-units m --threshold 50 --mask-file 4328.json 4328.mp4
rem %TR% --default high-contrast-small.def --fps 240 --view-rotation -90 --view-scale 1172.06 --csv-units m --threshold 50 --mask-file 4329.json 4329.mp4
rem %TR% --default high-contrast-small.def --fps 240 --view-rotation -90 --view-scale 1166.48 --csv-units m --threshold 50 --mask-file 4330.json 4330.mp4
rem %TR% --default high-contrast-small.def --fps 240 --view-rotation -90 --view-scale 1173.92 --csv-units m --threshold 100 --mask-file 4331.json 4331.mp4
rem %TR% --default high-contrast-small.def --fps 240 --view-rotation -90 --view-scale 1169.00 --csv-units m --threshold 50 --mask-file 4332.json 4332.mp4
rem %TR% --default knn-small.def --view-rotation -90 --view-scale 1209.67 --csv-units m --first-tracking-frame 110 4333.mp4
rem %TR% --default knn-small.def --view-scale 1200.65 --csv-units m 4338.mp4
rem %TR% --default knn-small.def --view-scale 1222.46 --csv-units m --mask-file 4394.json 4394.mp4
rem %TR% --default knn-small.def --view-scale 1220.93 --csv-units m --first-tracking-frame 100 4395.mp4
rem %TR% --default knn-small.def --view-scale 1223.81 --csv-units m 4396.mp4
rem %TR% --default knn-small.def --view-scale 1222.55 --csv-units m --mask-file 4397.json 4397.mp4
rem %TR% --default knn-small.def --view-scale 1223.81 --csv-units m 4398.mp4
rem %TR% --default knn-small.def --view-scale 1228.16 --csv-units m 4399.mp4
rem %TR% --default knn-small.def --view-scale 1225.45 --csv-units m 4400.mp4
rem %TR% --default knn-small.def --view-scale 1225.38 --csv-units m 4401.mp4
rem %TR% --default knn-small.def --view-scale 1226.90 --csv-units m --mask-file 4402.json 4402.mp4
rem %TR% --default knn-small.def --view-scale 1196.71 --csv-units m 4403.mp4
rem %TR% --default knn-small.def --view-scale 1192.63 --csv-units m 4404.mp4
rem %TR% --default knn-small.def --view-scale 1192.84 --csv-units m 4405.mp4
rem %TR% --default knn-small.def --view-scale 1193.86 --csv-units m 4406.mp4
rem %TR% --default knn-small.def --view-scale 1195.16 --csv-units m --mask-file 4407.json --foreground-segmenter KNN:500:600:false --min-contour 40 --age-weighting .2 4407.mp4
rem %TR% --default knn-small.def --view-scale 1190.88 --csv-units m 4408.mp4
rem %TR% --default knn-small.def --view-scale 1195.19 --csv-units m --min-contour 100 --max-contour 1000 4409.mp4
rem %TR% --default knn-small.def --view-scale 1198.33 --csv-units m --min-contour 100 --max-contour 1000 4410.mp4
rem %TR% --default knn-small.def --fps 120 --view-scale 1816.73 --csv-units m 4450.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1823.25 --csv-units m 4451.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1833.75 --csv-units m 4452.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1823.48 --csv-units m 4453.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1877.05 --csv-units m 4454.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1831.52 --csv-units m 4455.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1846.94 --csv-units m 4456.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1831.93 --csv-units m 4457.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1790.28 --csv-units m --min-contour 400 --max-contour 1200 4458.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1794.96 --csv-units m 4459.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1781.83 --csv-units m 4460.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1768.53 --csv-units m 4461.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1788.21 --csv-units m 4462.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1803.94 --csv-units m 4463.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1835.84 --csv-units m 4464.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1844.41 --csv-units m 4465.mp4
rem %TR% --default knn-medium.def --fps 120 --view-scale 1850.93 --csv-units m 4466.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1223.85 --csv-units m 4467.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1201.02 --csv-units m 4468.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1209.49 --csv-units m --foreground-segmenter KNN:500:600:false --min-contour 20 --mask-file 4469.json 4469.mp4
rem %TR% --default high-contrast-small.def --threshold 20 --fps 240 --view-scale 1203.55 --csv-units m --mask true 4470.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1195.30 --csv-units m 4471.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1201.58 --csv-units m 4472.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1206.89 --csv-units m 4473.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1210.58 --csv-units m 4474.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1202.09 --csv-units m 4613.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1176.27 --csv-units m 4614.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1176.24 --csv-units m 4615.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1177.89 --csv-units m 4616.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1179.51 --csv-units m 4617.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1177.97 --csv-units m 4618.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1196.49 --csv-units m 4621.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1193.68 --csv-units m 4622.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1164.58 --csv-units m 4623.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1165.95 --csv-units m 4624.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1151.79 --csv-units m 4625.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1150.18 --csv-units m 4627.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1192.14 --csv-units m 4628.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1164.54 --csv-units m 4629.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1163.24 --csv-units m 4630.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1163.09 --csv-units m 4631.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1181.86 --csv-units m 4632.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1183.27 --csv-units m 4633.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1196.38 --csv-units m 4634.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1193.56 --csv-units m 4635.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1196.51 --csv-units m 4636.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1194.99 --csv-units m 4637.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1193.41 --csv-units m 4638.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1210.83 --csv-units m 4639.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1210.77 --csv-units m 4640.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1215.15 --csv-units m 4641.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1218.32 --csv-units m 4642.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1223.84 --csv-units m 4643.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1184.79 --csv-units m 4644.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1180.37 --csv-units m 4645.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1207.91 --csv-units m 4646.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1206.43 --csv-units m 4647.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1178.93 --csv-units m 4648.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1179.11 --csv-units m 4649.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1183.34 --csv-units m 4650.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1197.86 --csv-units m 4651.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1183.43 --csv-units m 4652.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1173.21 --csv-units m 4653.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1171.87 --csv-units m 4654.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1205.01 --csv-units m 4655.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1200.70 --csv-units m 4656.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1194.84 --csv-units m 4657.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1206.45 --csv-units m 4658.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1202.10 --csv-units m 4659.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1230.05 --csv-units m --age-weighting .1 --foreground-segmenter KNN:700:500:true 4660.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1225.53 --csv-units m 4661.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1222.58 --csv-units m 4662.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1219.47 --csv-units m 4663.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1220.93 --csv-units m 4664.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1209.32 --csv-units m 4665.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1210.79 --csv-units m 4666.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1196.32 --csv-units m 4667.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1194.85 --csv-units m 4668.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1154.32 --csv-units m 4669.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1154.29 --csv-units m 4670.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1154.30 --csv-units m 4671.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1154.29 --csv-units m 4672.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1190.62 --csv-units m 4673.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1186.15 --csv-units m 4674.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1174.57 --csv-units m 4675.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1173.41 --csv-units m 4676.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1171.80 --csv-units m 4677.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1193.39 --csv-units m 4678.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1176.08 --csv-units m 4679.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1176.07 --csv-units m 4680.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1174.89 --csv-units m 4681.mp4
rem %TR% --default high-contrast.def --fps 240 --view-scale 1180.51 --csv-units m 4682.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1176.12 --csv-units m 4683.mp4
rem %TR% --default high-contrast.def --fps 240 --view-scale 1177.82 --csv-units m 4684.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1178.06 --csv-units m 4685.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1180.56 --csv-units m 4686.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1180.75 --csv-units m 4687.mp4
rem %TR% --default high-contrast.def --fps 240 --view-scale 1178.92 --csv-units m 4688.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1197.77 --csv-units m 4689.mp4
rem %TR% --default high-contrast.def --fps 240 --view-scale 1193.39 --csv-units m 4690.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1174.74 --csv-units m 4691.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1176.24 --csv-units m 4692.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1179.20 --csv-units m 4693.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1173.17 --csv-units m 4694.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1206.44 --csv-units m 4695.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1209.32 --csv-units m 4696.mp4
rem %TR% --default high-contrast.def --fps 240 --view-scale 1173.47 --csv-units m 4697.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1176.71 --csv-units m 4698.mp4
rem %TR% --default high-contrast.def --threshold 80 --fps 240 --view-scale 1173.47 --csv-units m 4699.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1176.76 --csv-units m 4700.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1153.13 --csv-units m 4701.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1157.71 --csv-units m 4702.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1171.90 --csv-units m 4703.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1173.51 --csv-units m 4704.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1152.84 --csv-units m 4705.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1154.32 --csv-units m 4706.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1167.55 --csv-units m 4707.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1183.90 --csv-units m 4708.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1283.66 --csv-units m 4709.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1218.26 --csv-units m 4710.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1216.06 --csv-units m 4711.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1216.30 --csv-units m 4712.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1207.55 --csv-units m 4713.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1211.55 --csv-units m 4714.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1208.42 --csv-units m 4715.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1209.78 --csv-units m 4716.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1199.23 --csv-units m 4717.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1207.88 --csv-units m 4718.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1225.88 --csv-units m 4719.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1225.67 --csv-units m 4720.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1223.08 --csv-units m 4721.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1224.03 --csv-units m 4722.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1210.90 --csv-units m 4723.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1202.92 --csv-units m 4724.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1207.98 --csv-units m 4725.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1212.64 --csv-units m 4726.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1234.39 --csv-units m 4728.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1173.72 --csv-units m 4729.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1171.78 --csv-units m 4730.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1223.93 --csv-units m 4731.mp4
rem %TR% --default knn-small.def --fps 240 --view-scale 1226.74 --csv-units m 4732.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1228.53 --csv-units m 4733.mp4
rem %TR% --default knn-medium.def --fps 240 --view-scale 1289.37 --csv-units m 4734.mp4
