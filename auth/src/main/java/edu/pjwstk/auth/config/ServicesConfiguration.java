package edu.pjwstk.auth.config;

import edu.pjwstk.auth.persistence.repository.EmailVerificationRepository;
import edu.pjwstk.auth.usecase.SendEmailVerificationCodeUseCase;
import edu.pjwstk.auth.usecase.impl.SendEmailVerificationCodeUseCaseImpl;
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
}
