package pl.gamilife.auth.service.impl;

import pl.gamilife.auth.models.ForgotPasswordCode;
import pl.gamilife.auth.repository.JpaForgotPasswordCodeRepository;
import pl.gamilife.auth.service.ForgotPasswordCodeService;
import lombok.RequiredArgsConstructor;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ForgotPasswordCodeServiceImpl implements ForgotPasswordCodeService {

    private final JpaForgotPasswordCodeRepository forgotPasswordCodeRepository;

    @Value("${spring.codes.forgot-password-code.expires-in}")
    private long forgotPasswordCodeTimeout;

    @Value("${spring.codes.forgot-password-code.resend-interval}")
    private long forgotPasswordCodeResendInterval;

    @Override
    public String generateAndSaveForgotPasswordCode(UUID userId) {
        String code = UUID.randomUUID().toString();

        ForgotPasswordCode forgotPasswordCode = new ForgotPasswordCode(
                UUID.randomUUID(),
                userId,
                hashCode(code),
                LocalDateTime.now(),
                LocalDateTime.now().plusSeconds(forgotPasswordCodeTimeout),
                false
        );

        forgotPasswordCodeRepository.save(forgotPasswordCode);

        return code;
    }

    @Override
    public String hashCode(String code) {
        return DigestUtils.sha256Hex(code);
    }

    @Override
    public void revokeAllActiveForgotPasswordCodesByUserId(UUID userId) {
        forgotPasswordCodeRepository.revokeAllActiveForgotPasswordCodesByUserId(userId);
    }

    @Override
    public boolean checkIfCanResendForgotPasswordCode(List<ForgotPasswordCode> codes) {
        // Do not resend a code if user has a non revoked code that will not expire
        // in the duration of the resend interval period
        return codes.isEmpty() ||
                codes.getFirst()
                        .getExpiresAt()
                        .minusSeconds(forgotPasswordCodeResendInterval)
                        .isBefore(LocalDateTime.now());
    }


}
