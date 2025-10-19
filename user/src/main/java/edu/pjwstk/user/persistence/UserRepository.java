package edu.pjwstk.user.persistence;

import edu.pjwstk.user.domain.User;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserRepository {

    User save(User user);

    List<User> getAllUsers();

    Optional<User> getUserByEmailWithPassword(String email);

    Optional<User> getUserByEmail(String email);

    Optional<User> getUserById(UUID id);

    void updateUserEmail(UUID id, String newEmail);

    void updateUserEmailVerificationStatus(UUID userId, boolean newStatus);
}
