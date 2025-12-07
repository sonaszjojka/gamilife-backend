package pl.gamilife.user.controllers;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.groups.GroupApi;
import edu.pjwstk.api.groups.dto.FindAllGroupsByUserIdWhereUserIsMemberResult;
import edu.pjwstk.commonweb.CookieUtil;
import edu.pjwstk.user.dto.request.ChangeUserPasswordRequest;
import edu.pjwstk.user.dto.request.EditUserRequest;
import edu.pjwstk.user.dto.response.*;
import edu.pjwstk.user.dto.response.UserDetailsResponse;
import edu.pjwstk.user.dto.service.*;
import edu.pjwstk.user.usecase.*;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.FindAllGroupsByUserIdWhereUserIsMemberResult;
import pl.gamilife.shared.web.util.CookieUtil;
import pl.gamilife.user.dto.request.ChangeUserPasswordRequest;
import pl.gamilife.user.dto.response.CurrentUserInfoResponse;
import pl.gamilife.user.dto.response.GetUsersResult;
import pl.gamilife.user.dto.response.UserDetailsResponse;
import pl.gamilife.user.dto.service.ChangeUserPasswordCommand;
import pl.gamilife.user.dto.service.GetUsersCommand;
import pl.gamilife.user.dto.service.UserDetailsDto;
import pl.gamilife.user.usecase.ChangeUserPasswordUseCase;
import pl.gamilife.user.usecase.CompleteOnboardingUseCase;
import pl.gamilife.user.usecase.GetUserDetailsUseCase;
import pl.gamilife.user.usecase.GetUsersUseCase;

import java.util.UUID;

@SecurityRequirement(name = "accessToken")
@RestController
@RequestMapping("/api/v1/users")
@AllArgsConstructor
public class UserController {

    private GetUserDetailsUseCase getUserDetailsUseCase;
    private ChangeUserPasswordUseCase changeUserPasswordUseCase;
    private GetUsersUseCase getUsersUseCase;
    private CompleteOnboardingUseCase completeOnboardingUseCase;
    private EditUserUseCase editUserUseCase;
    private CookieUtil cookieUtil;
    private GroupApi groupsApi;

    @GetMapping("/{userId}")
    //TODO: isProfilePrivate
    public ResponseEntity<UserDetailsResponse> getCurrentUserDetails(
            @PathVariable UUID userId,
            Authentication authentication
    ) {
        String requesterEmail = authentication.getName();
        UserDetailsResponse result = getUserDetailsUseCase.execute(requesterEmail, userId);

        return ResponseEntity.ok(result);
    }

    @PutMapping("/{userId}")
    public ResponseEntity<EditUserResult> editUser(
            @RequestBody @Valid EditUserRequest request,
            @PathVariable("userId") UUID userId) {

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

    @PatchMapping("/{userId}/password")
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<CurrentUserInfoResponse> changePassword(
            @PathVariable UUID userId,
            @RequestBody ChangeUserPasswordRequest request,
            HttpServletResponse response) {
        AuthTokens tokens = changeUserPasswordUseCase.execute(new ChangeUserPasswordCommand(
                userId, request.oldPassword(), request.newPassword()
        ));

        ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(tokens.accessToken());
        ResponseCookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(tokens.refreshToken());

        response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
        response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());

        return ResponseEntity.noContent().build();
    }

    @PutMapping("/{userId}/complete-onboarding")
    public ResponseEntity<UserDetailsResponse> completeOnboarding(
            @PathVariable UUID userId
    ) {
        UserDetailsDto dto = completeOnboardingUseCase.execute(userId);
        return ResponseEntity.ok(UserFullDetailsResponse.from(dto));
    }

    @GetMapping("/{userId}/groups")
    public ResponseEntity<FindAllGroupsByUserIdWhereUserIsMemberResult> getAllGroupsByUserId(
            @PathVariable("userId") UUID userId,

            @RequestParam(required = false) String joinCode,

            @RequestParam(required = false) Integer groupType,

            @RequestParam(required = false) String groupName,

            @RequestParam(defaultValue = "0")
            @Min(0) Integer page,

            @RequestParam(defaultValue = "10")
            @Min(1) @Max(100) Integer size

    ) {
        FindAllGroupsByUserIdWhereUserIsMemberResult response = groupsApi
                .findAllGroupsByUserIdWhereUserIsMember(userId, page, size, joinCode, groupType, groupName);
        return ResponseEntity.ok(response);
    }

    @GetMapping
    public ResponseEntity<GetUsersResult> getUsers(
            @RequestParam(required = false) String username,
            @RequestParam(defaultValue = "0") @Min(0) Integer page,
            @RequestParam(defaultValue = "10") @Min(1) @Max(100) Integer size
    ) {
        GetUsersCommand cmd = new GetUsersCommand(username, page, size);
        GetUsersResult result = getUsersUseCase.execute(cmd);
        return ResponseEntity.ok(result);
    }
}
