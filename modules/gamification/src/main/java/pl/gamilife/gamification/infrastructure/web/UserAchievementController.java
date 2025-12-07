package pl.gamilife.gamification.infrastructure.web;

import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import pl.gamilife.gamification.application.usecase.getalluserachievements.GetAllUserAchievementsCommand;
import pl.gamilife.gamification.application.usecase.getalluserachievements.GetAllUserAchievementsResult;
import pl.gamilife.gamification.application.usecase.getalluserachievements.GetAllUserAchievementsUseCase;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/users/{userId}/achievements")
public class UserAchievementController {

    private final GetAllUserAchievementsUseCase getAllUserAchievementsUseCase;

    @GetMapping
    //TODO: isProfilePrivate
    public ResponseEntity<GetAllUserAchievementsResult> getUserAchievements(@PathVariable UUID userId) {
        return ResponseEntity.ok(getAllUserAchievementsUseCase.execute(
                new GetAllUserAchievementsCommand(userId)
        ));
    }
}
