package edu.pjwstk.auth.config;

import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.repository.JpaForgotPasswordCodeRepository;
import edu.pjwstk.auth.usecase.SendEmailVerificationCodeUseCase;
import edu.pjwstk.auth.usecase.SendForgotPasswordTokenUseCase;
import edu.pjwstk.auth.usecase.impl.SendEmailVerificationCodeUseCaseImpl;
import edu.pjwstk.auth.usecase.impl.SendForgotPasswordTokenUseCaseImpl;
import edu.pjwstk.auth.util.ForgotPasswordCodeUtil;
import edu.pjwstk.auth.util.VerificationCodeUtil;
import edu.pjwstk.api.emailSender.EmailSenderApi;
import edu.pjwstk.api.user.UserApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ServicesConfiguration {

    @Bean
    public SendEmailVerificationCodeUseCase sendEmailVerificationCodeUseCase(
            UserApi userApi,
            EmailSenderApi emailSenderApi,
            JpaEmailVerificationRepository emailVerificationRepository,
            VerificationCodeUtil verificationCodeUtil,
            @Value("${spring.codes.verification-code.expires-in}")
            long emailVerificationTimeout,
            @Value("${spring.codes.verification-code.resend-interval}")
            long emailVerificationResendInterval) {
        return new SendEmailVerificationCodeUseCaseImpl(
                userApi,
                emailSenderApi,
                emailVerificationRepository,
                verificationCodeUtil,
                emailVerificationTimeout,
                emailVerificationResendInterval
        );
    }

    @Bean
    public SendForgotPasswordTokenUseCase generateAndSendForgotPasswordTokenUseCase(
            ForgotPasswordCodeUtil forgotPasswordCodeUtil,
            UserApi userApi,
            EmailSenderApi emailSenderApi,
            JpaForgotPasswordCodeRepository forgotPasswordCodeRepository,
            @Value("${spring.codes.forgot-password-code.expires-in}")
            long forgotPasswordCodeTimeout,
            @Value("${spring.codes.forgot-password-code.resend-interval}")
            long forgotPasswordCodeResendInterval) {
        return new SendForgotPasswordTokenUseCaseImpl(
                forgotPasswordCodeUtil,
                userApi,
                emailSenderApi,
                forgotPasswordCodeRepository,
                forgotPasswordCodeTimeout,
                forgotPasswordCodeResendInterval

        );
    }
}
