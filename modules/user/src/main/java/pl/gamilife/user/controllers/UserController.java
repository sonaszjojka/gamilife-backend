package pl.gamilife.user.controllers;

import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.web.security.annotation.AuthenticatedUserIsOwner;
import pl.gamilife.user.dto.request.EditUserRequest;
import pl.gamilife.user.dto.service.UserDetails;
import pl.gamilife.user.usecase.CompleteOnboardingUseCase;
import pl.gamilife.user.usecase.edituser.EditUserCommand;
import pl.gamilife.user.usecase.edituser.EditUserResult;
import pl.gamilife.user.usecase.edituser.EditUserUseCase;
import pl.gamilife.user.usecase.getusers.GetUsersCommand;
import pl.gamilife.user.usecase.getusers.GetUsersResult;
import pl.gamilife.user.usecase.getusers.GetUsersUseCase;

import java.util.UUID;

@SecurityRequirement(name = "accessToken")
@RestController
@RequestMapping("/api/v1/users")
@AllArgsConstructor
public class UserController {

    private GetUsersUseCase getUsersUseCase;
    private CompleteOnboardingUseCase completeOnboardingUseCase;
    private EditUserUseCase editUserUseCase;

    @PutMapping("/{userId}")
    @AuthenticatedUserIsOwner
    public ResponseEntity<EditUserResult> editUser(
            @RequestBody @Valid EditUserRequest request,
            @PathVariable UUID userId) {

        EditUserResult response = editUserUseCase.execute(new EditUserCommand(
                userId,
                request.firstName(),
                request.lastName(),
                request.username(),
                request.dateOfBirth(),
                request.sendBudgetReports(),
                request.isProfilePublic()
        ));

        return ResponseEntity.ok(response);
    }

    @PutMapping("/{userId}/complete-onboarding")
    @AuthenticatedUserIsOwner
    public ResponseEntity<UserDetails> completeOnboarding(
            @PathVariable UUID userId
    ) {
        UserDetails dto = completeOnboardingUseCase.execute(userId);
        return ResponseEntity.ok(dto);
    }

    @GetMapping
    public ResponseEntity<Page<GetUsersResult>> getUsers(
            @RequestParam(required = false) String username,
            @RequestParam(defaultValue = "0") @Min(0) Integer page,
            @RequestParam(defaultValue = "10") @Min(1) @Max(100) Integer size
    ) {
        GetUsersCommand cmd = new GetUsersCommand(username, page, size);
        Page<GetUsersResult> result = getUsersUseCase.execute(cmd);
        return ResponseEntity.ok(result);
    }
}
