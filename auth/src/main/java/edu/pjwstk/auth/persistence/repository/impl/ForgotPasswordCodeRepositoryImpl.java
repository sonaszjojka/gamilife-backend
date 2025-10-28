package edu.pjwstk.auth.persistence.repository.impl;

import edu.pjwstk.auth.domain.ForgotPasswordCode;
import edu.pjwstk.auth.persistence.entity.ForgotPasswordCodeEntity;
import edu.pjwstk.auth.persistence.mapper.ForgotPasswordCodeMapper;
import edu.pjwstk.auth.persistence.repository.ForgotPasswordCodeRepository;
import edu.pjwstk.auth.persistence.repository.jpa.JpaForgotPasswordCodeRepository;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class ForgotPasswordCodeRepositoryImpl implements ForgotPasswordCodeRepository {

    private final JpaForgotPasswordCodeRepository jpaRepository;

    @Override
    public void create(ForgotPasswordCode emailVerification) {
        ForgotPasswordCodeEntity entity = ForgotPasswordCodeMapper.toEntity(emailVerification);
        jpaRepository.save(entity);
    }

    @Override
    public Optional<ForgotPasswordCode> findByCode(String code) {
        Optional<ForgotPasswordCodeEntity> entity = jpaRepository.findByCode(code);

        return entity.map(ForgotPasswordCodeMapper::toDomain);
    }

    @Override
    public List<ForgotPasswordCode> findByUserIdAndNotRevokedOrderByIssuedAt(UUID userId) {
        List<ForgotPasswordCodeEntity> list = jpaRepository
                .findByUserIdAndRevoked(userId, false, Sort.by(Sort.Direction.DESC, "issuedAt"));

        return list.stream().map(ForgotPasswordCodeMapper::toDomain).toList();
    }
}
