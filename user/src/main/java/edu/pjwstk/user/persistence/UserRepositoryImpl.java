package edu.pjwstk.user.persistence;

import edu.pjwstk.user.domain.User;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public class UserRepositoryImpl implements UserRepository {

    private final JpaUserRepository jpaUserRepository;

    public UserRepositoryImpl(JpaUserRepository jpaUserRepository) {
        this.jpaUserRepository = jpaUserRepository;
    }

    @Override
    public User save(User user) {
        UserEntity userEntity = UserMapper.toEntity(user);
        UserEntity createdUserEntity = jpaUserRepository.save(userEntity);
        return UserMapper.toDomain(createdUserEntity);
    }

    @Override
    public List<User> getAllUsers() {
        return jpaUserRepository
                .findAll()
                .stream()
                .map(UserMapper::toDomain)
                .toList();
    }

    @Override
    public Optional<User> getUserByEmailWithPassword(String email) {
        return jpaUserRepository.findByEmailWithPassword(email).map(UserMapper::toDomain);
    }

    @Override
    public Optional<User> getUserByEmail(String email) {
        return jpaUserRepository.findByEmail(email).map(UserMapper::toDomain);
    }

    @Override
    public Optional<User> getUserById(UUID id) {
        return jpaUserRepository.findById(id).map(UserMapper::toDomain);
    }

    @Override
    public void updateUserEmail(UUID id, String newEmail) {
        jpaUserRepository.updateUserEmail(id, newEmail);
    }

    @Override
    public void updateUserEmailVerificationStatus(UUID userId, boolean newStatus) {
        jpaUserRepository.updateIsEmailVerifiedById(userId, newStatus);
    }

}
