package pl.gamilife.user.persistence;

import pl.gamilife.shared.kernel.architecture.Page;

import java.util.Optional;
import java.util.UUID;

public interface UserRepository {

    User save(User user);

    Optional<User> getUserByEmail(String email);

    Optional<User> getUserById(UUID id);

    void updateUserEmail(UUID id, String newEmail);

    void updateUserEmailVerificationStatus(UUID userId, boolean newStatus);

    void updateUserMoney(UUID userId, int newMoney);

    Page<User> findAll(String username, int page, int size);

    boolean existsByUsername(String username);

}
