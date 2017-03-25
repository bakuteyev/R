
df <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat")

colnames(df) <-  c('frequency',         # 1. Частота, Гц.
                    'angle',            # 2. Угол атаки, градусы.
                    'chord_length',     # 3. Длина хорды профиля лопатки, м.
                    'fs_velocity',      # 4. Скорость набегающего потока, м/с.
                    'ssd_thickness',    # 5. Толщина вытеснения на выпуклой стороне (спинке) лопатки, м.
                    'sound_level')      # 6. Уровень громкости, дБ.

head(df)

correlations <- as.data.frame(as.table(cor(df[,c('frequency',     
                                                'angle',        
                                                'chord_length', 
                                                'fs_velocity',  
                                                'ssd_thickness')])))

correlations <- correlations[correlations$Var1 != correlations$Var2,]
correlations <- correlations[abs(correlations$Freq) > 0.5, ]

correlations

df <- df[,c('frequency',           
            'chord_length', 
            'fs_velocity',  
            'ssd_thickness',
            'sound_level')]

linear_model <- lm(sound_level ~ frequency + chord_length + fs_velocity + ssd_thickness, data=df)

summary(linear_model)

newdf <- as.data.frame.list(colMeans(df))
newdf

options(repr.plot.width = 10, repr.plot.height = 5)
hist(linear_model$residuals, probability = T)

shapiro.test(linear_model$residuals)

predict(linear_model, newdf, interval="predict") 

library("quantreg")

quantile_model <- rq(sound_level ~ frequency + chord_length + fs_velocity + ssd_thickness, data=df, tau=0.25)
summary(quantile_model)

quantile_model <- rq(sound_level ~ frequency + chord_length + fs_velocity + ssd_thickness, data=df, tau=0.5)
summary(quantile_model)

quantile_model <- rq(sound_level ~ frequency + chord_length + fs_velocity + ssd_thickness, data=df, tau=0.75)
summary(quantile_model)

newdf <- as.data.frame.list(colMeans(df))
newdf

hist(quantile_model$residuals, probability = T)

shapiro.test(quantile_model$residuals)

predict(quantile_model, newdf, interval=c("confidence")) 
