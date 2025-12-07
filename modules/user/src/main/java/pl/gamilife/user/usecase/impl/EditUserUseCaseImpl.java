package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.UserAlreadyExistsException;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.dto.service.EditUserCommand;
import edu.pjwstk.user.dto.service.EditUserResult;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.EditUserUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@AllArgsConstructor
public class EditUserUseCaseImpl implements EditUserUseCase {

    private final UserRepository userRepository;

    @Override
    @Transactional
    public EditUserResult execute(EditUserCommand cmd) {
        User user = getUser(cmd.userId());

        if (!user.getUsername().equals(cmd.username()) &&
                userRepository.existsByUsername(cmd.username())) {
            throw new UserAlreadyExistsException("Username is already taken!");
        }

        user.setFirstName(cmd.firstName());
        user.setLastName(cmd.lastName());
        user.setUsername(cmd.username());
        user.setDateOfBirth(cmd.dateOfBirth());
        user.setSendBudgetReports(cmd.sendBudgetReports());
        user.setProfilePublic(cmd.isProfilePublic());

        User savedUser = userRepository.save(user);

        return buildEditUserResult(savedUser);
    }

    private User getUser(UUID userId) {
        return userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User with id: " + userId + " not found!"));
    }

    private EditUserResult buildEditUserResult(User user) {
        return EditUserResult.builder()
                .id(user.getId())
                .firstName(user.getFirstName())
                .lastName(user.getLastName())
                .email(user.getEmail())
                .username(user.getUsername())
                .dateOfBirth(user.getDateOfBirth())
                .sendBudgetReports(user.isSendBudgetReports())
                .isProfilePublic(user.isProfilePublic())
                .isEmailVerified(user.isEmailVerified())
                .build();
    }
}
