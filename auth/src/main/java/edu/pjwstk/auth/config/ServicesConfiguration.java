package edu.pjwstk.auth.config;

import edu.pjwstk.api.emailSender.EmailSenderApi;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.auth.repository.JpaForgotPasswordCodeRepository;
import edu.pjwstk.auth.service.ForgotPasswordCodeService;
import edu.pjwstk.auth.usecase.SendForgotPasswordTokenUseCase;
import edu.pjwstk.auth.usecase.impl.SendForgotPasswordTokenUseCaseImpl;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ServicesConfiguration {

    @Bean
    public SendForgotPasswordTokenUseCase generateAndSendForgotPasswordTokenUseCase(
            ForgotPasswordCodeService forgotPasswordCodeService,
            UserApi userApi,
            EmailSenderApi emailSenderApi,
            JpaForgotPasswordCodeRepository forgotPasswordCodeRepository,
            @Value("${spring.codes.forgot-password-code.expires-in}")
            long forgotPasswordCodeTimeout,
            @Value("${spring.codes.forgot-password-code.resend-interval}")
            long forgotPasswordCodeResendInterval) {
        return new SendForgotPasswordTokenUseCaseImpl(
                forgotPasswordCodeService,
                userApi,
                emailSenderApi,
                forgotPasswordCodeRepository,
                forgotPasswordCodeTimeout,
                forgotPasswordCodeResendInterval

        );
    }
}
