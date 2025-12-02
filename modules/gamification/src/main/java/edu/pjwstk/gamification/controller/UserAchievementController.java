package edu.pjwstk.gamification.controller;

import edu.pjwstk.gamification.usecase.getalluserachievements.GetAllUserAchievementsCommand;
import edu.pjwstk.gamification.usecase.getalluserachievements.GetAllUserAchievementsResult;
import edu.pjwstk.gamification.usecase.getalluserachievements.GetAllUserAchievementsUseCase;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/users/{userId}/achievements")
public class UserAchievementController {

    private final GetAllUserAchievementsUseCase getAllUserAchievementsUseCase;

    @GetMapping
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<GetAllUserAchievementsResult> getUserAchievements(@PathVariable UUID userId) {
        return ResponseEntity.ok(getAllUserAchievementsUseCase.execute(
                new GetAllUserAchievementsCommand(userId)
        ));
    }
}
