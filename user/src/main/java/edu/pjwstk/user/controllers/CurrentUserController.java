package edu.pjwstk.user.controllers;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.user.dto.response.CurrentUserInfoResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@SecurityRequirement(name = "accessToken")
@RestController
@RequestMapping("/api/v1/me")
public class CurrentUserController {

    private final AuthApi authApi;

    public CurrentUserController(AuthApi authApi) {
        this.authApi = authApi;
    }

    @GetMapping
    public ResponseEntity<CurrentUserInfoResponse> getCurrentUserInfo() {
        CurrentUserDto dto = authApi.getCurrentUser().get();

        return ResponseEntity.ok(new CurrentUserInfoResponse(
                dto.email()
        ));
    }
}
