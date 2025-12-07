package pl.gamilife.user.usecase.edituser;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.UserAlreadyExistsException;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;

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
