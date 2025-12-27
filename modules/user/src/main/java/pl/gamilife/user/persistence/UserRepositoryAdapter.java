package pl.gamilife.user.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.user.persistence.jpa.JpaUserRepository;
import pl.gamilife.user.persistence.specification.UserSpecificationBuilder;

import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class UserRepositoryAdapter implements UserRepository {

    private final JpaUserRepository jpaUserRepository;
    private final UserSpecificationBuilder userSpecificationBuilder;

    @Override
    public User save(User user) {
        return jpaUserRepository.save(user);
    }

    @Override
    public Optional<User> getUserByEmail(String email) {
        return jpaUserRepository.findByEmail(email);
    }

    @Override
    public Optional<User> getUserById(UUID id) {
        return jpaUserRepository.findById(id);
    }

    @Override
    public void updateUserEmail(UUID id, String newEmail) {
        jpaUserRepository.updateUserEmail(id, newEmail);
    }

    @Override
    public void updateUserEmailVerificationStatus(UUID userId, boolean newStatus) {
        jpaUserRepository.updateIsEmailVerifiedById(userId, newStatus);
    }

    @Override
    public void updateUserMoney(UUID userId, int newMoney) {
        jpaUserRepository.updateMoneyById(newMoney, userId);
    }

    public Page<User> findAll(String username, int page, int size) {
        org.springframework.data.domain.Page<User> result = jpaUserRepository.findAll(
                userSpecificationBuilder.build(username),
                PageRequest.of(page, size, Sort.by("username").ascending())
        );

        return new Page<>(
                result.getContent(),
                result.getTotalElements(),
                result.getTotalPages(),
                result.getNumber(),
                result.getSize()
        );
    }

    @Override
    public boolean existsByUsername(String username) {
        return jpaUserRepository.existsByUsername(username);
    }

}
