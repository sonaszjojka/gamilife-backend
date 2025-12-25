package pl.gamilife.gamification.infrastructure.web;

import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import pl.gamilife.gamification.application.usecase.getgamificationuser.GetGamificationUserCommand;
import pl.gamilife.gamification.application.usecase.getgamificationuser.GetGamificationUserResult;
import pl.gamilife.gamification.application.usecase.getgamificationuser.GetGamificationUserUseCase;
import pl.gamilife.shared.web.security.annotation.AllowUnverified;
import pl.gamilife.shared.web.security.annotation.AuthenticatedUserIsOwner;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/gamification-users")
public class GamificationUserController {

    private final GetGamificationUserUseCase getGamificationUserUseCase;

    @GetMapping("/{userId}")
    @AllowUnverified
    @AuthenticatedUserIsOwner
    public ResponseEntity<GetGamificationUserResult> getGamificationUserDetails(@PathVariable UUID userId) {
        GetGamificationUserResult result = getGamificationUserUseCase.execute(new GetGamificationUserCommand(userId));
        return ResponseEntity.ok(result);
    }

}
