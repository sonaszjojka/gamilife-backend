package pl.gamilife.auth.domain.service.impl;

import lombok.RequiredArgsConstructor;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import pl.gamilife.auth.domain.model.EmailVerificationCode;
import pl.gamilife.auth.domain.port.repository.EmailVerificationRepository;
import pl.gamilife.auth.domain.service.EmailVerificationService;
import pl.gamilife.shared.kernel.event.EmailVerificationRequestedEvent;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class EmailVerificationServiceImpl implements EmailVerificationService {

    private final EmailVerificationRepository emailVerificationRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Value("${spring.codes.verification-code.expires-in}")
    private long emailVerificationTimeout;

    @Value("${spring.codes.verification-code.resend-interval}")
    private long emailVerificationResendInterval;

    @Value("${app.frontend-urls.email-verification-url}")
    private String emailVerificationUrl;

    @Value("${app.frontend-urls.main-url}")
    private String appUrl;

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
        String verificationLink = String.format("%s%s?code=%s", appUrl, emailVerificationUrl, code);
        eventPublisher.publishEvent(new EmailVerificationRequestedEvent(userId, verificationLink));
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
