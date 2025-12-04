package pl.gamilife.user.persistence;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import pl.gamilife.user.domain.User;

import java.util.Optional;
import java.util.UUID;

public interface UserRepository {

    User save(User user);

    Optional<User> getUserByEmail(String email);

    Optional<User> getUserById(UUID id);

    void updateUserEmail(UUID id, String newEmail);

    void updateUserEmailVerificationStatus(UUID userId, boolean newStatus);

    void updateUserMoney(UUID userId, int newMoney);

    Page<User> findAll(Specification<UserEntity> userSpecification, PageRequest pageRequest);
}
