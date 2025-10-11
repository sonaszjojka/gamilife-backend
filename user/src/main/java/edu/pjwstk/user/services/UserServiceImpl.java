package edu.pjwstk.user.services;

import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.dto.RegisterUserApiDto;
import edu.pjwstk.common.userApi.exception.UserAlreadyExistsException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.dto.service.GetUserListItemDto;
import edu.pjwstk.user.persistence.UserRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Service
public class UserServiceImpl implements UserService, UserApi {

    private final UserRepository userRepository;

    public UserServiceImpl(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @Override
    public List<GetUserListItemDto> getAllUsers() {
        return userRepository
                .getAllUsers()
                .stream()
                .map(
                        user -> new GetUserListItemDto(
                                user.getEmail(),
                                user.getPassword(),
                                user.getUsername()
                        )
                )
                .toList();
    }

    @Override
    @Transactional
    public BasicUserInfoApiDto registerNewUser(RegisterUserApiDto user) {
        if (getUserByEmail(user.email()).isPresent()) {
            throw new UserAlreadyExistsException("This email address is already taken");
        }

        User newUser = new User(
                UUID.randomUUID(),
                user.firstName(),
                user.lastName(),
                user.email(),
                user.password(),
                user.username(),
                user.dateOfBirth(),
                0,
                0,
                user.sendBudgetReports(),
                user.isProfilePublic(),
                false
        );

        userRepository.save(newUser);

        return new BasicUserInfoApiDto(
                newUser.getId(),
                newUser.getEmail(),
                newUser.getPassword()
        );
    }

    @Override
    public Optional<BasicUserInfoApiDto> getUserById(UUID userId) {
        Optional<User> optionalUser = userRepository.getUserById(userId);

        if (optionalUser.isEmpty()) {
            return Optional.empty();
        }

        User user = optionalUser.get();

        return Optional.of(new BasicUserInfoApiDto(
                user.getId(),
                user.getEmail(),
                user.getPassword()
        ));
    }

    @Override
    public Optional<BasicUserInfoApiDto> getUserByEmail(String email) {
        Optional<User> optionalUser = userRepository.getUserByEmail(email);

        if (optionalUser.isEmpty()) {
            return Optional.empty();
        }

        User user = optionalUser.get();

        return Optional.of(new BasicUserInfoApiDto(
                user.getId(),
                user.getEmail(),
                user.getPassword()
        ));
    }

    @Override
    public void updateUserEmail(UUID userId, String newEmail) {
    }
}
