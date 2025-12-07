package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.dto.response.UserDetailsResponse;
import pl.gamilife.user.dto.response.UserFullDetailsResponse;
import pl.gamilife.user.dto.response.UserPrivateDetailsResponse;
import pl.gamilife.user.dto.response.UserPublicDetailsResponse;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.GetUserDetailsUseCase;

import java.util.UUID;

@Service
@AllArgsConstructor
public class GetUserDetailsUseCaseImpl implements GetUserDetailsUseCase {

    private final UserRepository userRepository;

    @Override
    @Transactional(readOnly = true)
    public UserDetailsResponse execute(String requesterEmail, UUID targetUserId) {

        User user = userRepository.getUserById(targetUserId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        boolean isOwner = requesterEmail.equals(user.getEmail());
        if (!isOwner && user.isProfilePublic()) {
            return UserPublicDetailsResponse.from(user);
        } else if (!isOwner) {
            return UserPrivateDetailsResponse.from(user);
        }

        return new UserFullDetailsResponse(
                user.getId(),
                user.getFirstName(),
                user.getLastName(),
                user.getEmail(),
                user.getUsername(),
                user.getDateOfBirth(),
                user.getExperience(),
                user.getLevel(),
                null, // TODO: Fix in future by adding layer above user and gamification
                user.getMoney(),
                user.isSendBudgetReports(),
                user.isProfilePublic(),
                user.isEmailVerified(),
                user.isTutorialCompleted()
        );
    }
}