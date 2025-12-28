package pl.gamilife.auth.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.auth.domain.model.EmailVerificationCode;
import pl.gamilife.auth.domain.port.repository.EmailVerificationRepository;
import pl.gamilife.auth.infrastructure.persistence.jpa.JpaEmailVerificationRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class EmailVerificationRepositoryAdapter implements EmailVerificationRepository {

    private final JpaEmailVerificationRepository jpaEmailVerificationRepository;

    @Override
    public void save(EmailVerificationCode emailVerification) {
        jpaEmailVerificationRepository.save(emailVerification);
    }

    @Override
    public void revokeAllActiveEmailVerificationCodesByUserId(UUID userId) {
        jpaEmailVerificationRepository.revokeAllActiveEmailVerificationCodesByUserId(userId);
    }

    @Override
    public Optional<EmailVerificationCode> findByUserIdAndCode(UUID userId, String hashedCode) {
        return jpaEmailVerificationRepository.findByUserIdAndCode(userId, hashedCode);
    }

    @Override
    public List<EmailVerificationCode> findByUserIdAndRevokedOrderByIssuedAtDesc(UUID userId, boolean revoked) {
        return jpaEmailVerificationRepository.findByUserIdAndRevokedOrderByIssuedAtDesc(userId, revoked);
    }
}
