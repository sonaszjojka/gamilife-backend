package edu.pjwstk.auth.persistence.repository.jpa;

import edu.pjwstk.auth.persistence.entity.RefreshTokenEntity;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;
import java.util.UUID;

public interface JpaRefreshTokenRepository extends JpaRepository<RefreshTokenEntity, Long> {

    Optional<RefreshTokenEntity> findByToken(String token);

    @Modifying
    @Transactional
    @Query("""
                UPDATE RefreshTokenEntity r
                SET r.revoked = :revoked
                WHERE r.id = :refId
            """)
    int updateRevokedById(
            @Param("refId") UUID refId,
            @Param("revoked") boolean revoked
    );
}
