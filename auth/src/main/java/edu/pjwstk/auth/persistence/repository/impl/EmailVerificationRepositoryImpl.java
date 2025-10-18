package edu.pjwstk.auth.persistence.repository.impl;

import edu.pjwstk.auth.domain.EmailVerification;
import edu.pjwstk.auth.persistence.entity.EmailVerificationEntity;
import edu.pjwstk.auth.persistence.mapper.EmailVerificationMapper;
import edu.pjwstk.auth.persistence.repository.EmailVerificationRepository;
import edu.pjwstk.auth.persistence.repository.jpa.JpaEmailVerificationRepository;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class EmailVerificationRepositoryImpl implements EmailVerificationRepository {

    private final JpaEmailVerificationRepository jpaRepository;

    @Override
    public void create(EmailVerification emailVerification) {
        EmailVerificationEntity entity = EmailVerificationMapper.toEntity(emailVerification);
        jpaRepository.save(entity);
    }

    @Override
    public Optional<EmailVerification> findByUserIdAndCode(UUID userId, String code) {
        Optional<EmailVerificationEntity> entity = jpaRepository.findByUserIdAndCode(userId, code);

        return entity.map(EmailVerificationMapper::toDomain);
    }

    @Override
    public List<EmailVerification> findByUserIdAndNotRevokedOrderByIssuedAt(UUID userId) {
        List<EmailVerificationEntity> list = jpaRepository
                .findByUserIdAndRevoked(userId, false, Sort.by(Sort.Direction.DESC, "issuedAt"));

        return list.stream().map(EmailVerificationMapper::toDomain).toList();
    }
}
