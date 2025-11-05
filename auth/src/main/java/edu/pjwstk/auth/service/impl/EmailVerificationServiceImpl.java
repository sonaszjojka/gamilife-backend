package edu.pjwstk.auth.service.impl;

import edu.pjwstk.api.emailSender.EmailSenderApi;
import edu.pjwstk.core.exception.common.application.EmailSendingException;
import edu.pjwstk.api.emailSender.MailContentType;
import edu.pjwstk.api.emailSender.MailDto;
import edu.pjwstk.auth.models.EmailVerificationCode;
import edu.pjwstk.auth.repository.JpaEmailVerificationRepository;
import edu.pjwstk.auth.service.EmailVerificationService;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
public class EmailVerificationServiceImpl implements EmailVerificationService {

    private final JpaEmailVerificationRepository emailVerificationRepository;
    private final EmailSenderApi emailSenderApi;

    @Value("${spring.codes.verification-code.expires-in}")
    private long emailVerificationTimeout;

    @Value("${spring.codes.verification-code.resend-interval}")
    private long emailVerificationResendInterval;

    public EmailVerificationServiceImpl(JpaEmailVerificationRepository emailVerificationRepository, EmailSenderApi emailSenderApi) {
        this.emailVerificationRepository = emailVerificationRepository;
        this.emailSenderApi = emailSenderApi;
    }

    @Override
    public String generateAndSaveEmailVerificationCode(UUID userId) {
        String code = UUID.randomUUID().toString();

        EmailVerificationCode emailVerification = new EmailVerificationCode(
                UUID.randomUUID(),
                userId,
                hashCode(code),
                LocalDateTime.now(),
                LocalDateTime.now().plusSeconds(emailVerificationTimeout),
                false
        );

        emailVerificationRepository.save(emailVerification);

        return code;
    }

    @Override
    public String hashCode(String code) {
        return DigestUtils.sha256Hex(code);
    }

    @Override
    public void revokeAllActiveEmailVerificationCodesByUserId(UUID userId) {
        emailVerificationRepository.revokeAllActiveEmailVerificationCodesByUserId(userId);
    }

    @Override
    public void sendEmailVerificationCode(UUID userId, String email, String code) {
        try {
            emailSenderApi.sendEmail(new MailDto(
                    email,
                    "Email verification code",
                    """
                            Hello!
                            
                            Here is your verification code:
                            """ +
                            code +
                            """
                                    \n
                                    Best regards,
                                    GamiLife Team
                                    """,
                    MailContentType.TEXT
            ));
        } catch (EmailSendingException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean checkIfCanResendEmailVerificationCode(List<EmailVerificationCode> codes) {
        // Do not resend a code if user has a non revoked code that will not expire
        // in the duration of the resend interval period
        return codes.isEmpty() || codes.getFirst()
                .getExpiresAt()
                .minusSeconds(emailVerificationResendInterval)
                .isBefore(LocalDateTime.now());
    }
}
