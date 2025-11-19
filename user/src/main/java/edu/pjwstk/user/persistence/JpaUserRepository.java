package edu.pjwstk.user.persistence;

import jakarta.transaction.Transactional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;
import java.util.UUID;

public interface JpaUserRepository extends JpaRepository<UserEntity, UUID> {
    @Query("""
                SELECT u
                FROM UserEntity u
                WHERE u.email = :email
                    AND u.password IS NOT NULL
            """)
    Optional<UserEntity> findByEmailWithPassword(@Param("email") String email);

    Optional<UserEntity> findByEmail(String email);

    @Modifying
    @Transactional
    @Query("""
                UPDATE UserEntity u
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
            UPDATE UserEntity u
            SET u.isEmailVerified = :status
            WHERE u.id = :id
            """)
    void updateIsEmailVerifiedById(
            @Param("id") UUID id,
            @Param("status") boolean status
    );

    @Transactional
    @Modifying
    @Query("update UserEntity u set u.money = ?1 where u.id = ?2")
    void updateMoneyById(int money, UUID id);
}
