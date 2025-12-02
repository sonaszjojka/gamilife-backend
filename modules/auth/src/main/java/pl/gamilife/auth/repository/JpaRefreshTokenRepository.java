package pl.gamilife.auth.repository;

import pl.gamilife.auth.models.RefreshToken;
import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;
import java.util.UUID;

public interface JpaRefreshTokenRepository extends JpaRepository<RefreshToken, UUID> {

    Optional<RefreshToken> findByToken(String token);

    @Modifying
    @Transactional
    @Query("""
                UPDATE RefreshToken r
                SET r.revoked = :revoked
                WHERE r.id = :refId
            """)
    void updateRevokedById(
            @Param("refId") UUID refId,
            @Param("revoked") boolean revoked
    );

    @Modifying
    @Transactional
    @Query("""
                UPDATE RefreshToken r
                SET r.revoked = true
                WHERE r.userId = :userId
                    AND r.revoked = false
                    AND r.expiresAt > CURRENT_TIMESTAMP
            """)
    void revokeAllActiveRefreshTokensByUserId(@Param("userId") UUID userId);
}
