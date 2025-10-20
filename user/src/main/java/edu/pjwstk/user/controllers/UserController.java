package edu.pjwstk.user.controllers;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.user.dto.response.CurrentUserInfoResponse;
import edu.pjwstk.user.usecase.GetUserByIdUseCase;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@SecurityRequirement(name = "accessToken")
@RestController
@RequestMapping("/api/v1/users")
@AllArgsConstructor
public class UserController {

    private GetUserByIdUseCase getUserByIdUseCase;

    @GetMapping("/{userId}")
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<CurrentUserInfoResponse> getCurrentUserInfo(@PathVariable UUID userId) {
        BasicUserInfoApiDto dto = getUserByIdUseCase.execute(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return ResponseEntity.ok(new CurrentUserInfoResponse(
                dto.email()
        ));
    }
}
