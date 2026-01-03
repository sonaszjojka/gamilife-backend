package pl.gamilife.auth.domain.service;

import pl.gamilife.auth.domain.model.EmailVerificationCode;

import java.util.List;
import java.util.UUID;

public interface EmailVerificationService {
    String generateAndSaveEmailVerificationCode(UUID userId);

    String hashCode(String code);

    void revokeAllActiveEmailVerificationCodesByUserId(UUID userId);

    void sendEmailVerificationCode(UUID userId, String code);

    boolean checkIfCanResendEmailVerificationCode(List<EmailVerificationCode> codes);
}
