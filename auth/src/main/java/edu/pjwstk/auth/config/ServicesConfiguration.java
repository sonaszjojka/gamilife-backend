package edu.pjwstk.auth.config;

import edu.pjwstk.auth.persistence.repository.EmailVerificationRepository;
import edu.pjwstk.auth.persistence.repository.ForgotPasswordCodeRepository;
import edu.pjwstk.auth.usecase.GenerateAndSendForgotPasswordTokenUseCase;
import edu.pjwstk.auth.usecase.SendEmailVerificationCodeUseCase;
import edu.pjwstk.auth.usecase.impl.GenerateAndSendForgotPasswordTokenUseCaseImpl;
import edu.pjwstk.auth.usecase.impl.SendEmailVerificationCodeUseCaseImpl;
import edu.pjwstk.auth.util.ForgotPasswordCodeUtil;
import edu.pjwstk.auth.util.VerificationCodeUtil;
import edu.pjwstk.common.emailSenderApi.EmailSenderApi;
import edu.pjwstk.common.userApi.UserApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ServicesConfiguration {

    @Bean
    public SendEmailVerificationCodeUseCase sendEmailVerificationCodeUseCase(
            UserApi userApi,
            EmailSenderApi emailSenderApi,
            EmailVerificationRepository emailVerificationRepository,
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
    public GenerateAndSendForgotPasswordTokenUseCase generateAndSendForgotPasswordTokenUseCase(
            ForgotPasswordCodeUtil forgotPasswordCodeUtil,
            UserApi userApi,
            EmailSenderApi emailSenderApi,
            ForgotPasswordCodeRepository forgotPasswordCodeRepository,
            @Value("${spring.codes.forgot-password-code.expires-in}")
            long forgotPasswordCodeTimeout,
            @Value("${spring.codes.forgot-password-code.resend-interval}")
            long forgotPasswordCodeResendInterval) {
        return new GenerateAndSendForgotPasswordTokenUseCaseImpl(
                forgotPasswordCodeUtil,
                userApi,
                emailSenderApi,
                forgotPasswordCodeRepository,
                forgotPasswordCodeTimeout,
                forgotPasswordCodeResendInterval

        );
    }
}
