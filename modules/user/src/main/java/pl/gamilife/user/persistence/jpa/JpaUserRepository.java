package pl.gamilife.user.persistence.jpa;

import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import pl.gamilife.user.persistence.User;

import java.util.Optional;
import java.util.UUID;

public interface JpaUserRepository extends JpaRepository<User, UUID>, JpaSpecificationExecutor<User> {

    Optional<User> findByEmail(String email);

    @Modifying
    @Transactional
    @Query("""
                UPDATE User u
                SET u.email = :newEmail
                WHERE u.id = :id
            """)
    void updateUserEmail(
            @Param("id") UUID id,
            @Param("newEmail") String newEmail
    );

    @Transactional
    @Modifying
    @Query("""
            UPDATE User u
            SET u.isEmailVerified = :status
            WHERE u.id = :id
            """)
    void updateIsEmailVerifiedById(
            @Param("id") UUID id,
            @Param("status") boolean status
    );

    @Transactional
    @Modifying
    @Query("update User u set u.money = ?1 where u.id = ?2")
    void updateMoneyById(int money, UUID id);

    boolean existsByUsername(String username);
}
