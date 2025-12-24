package pl.gamilife.app.controller;

import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import pl.gamilife.app.dto.UserDetailsResponse;
import pl.gamilife.app.service.UserDetailsService;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/users")
public class UserDetailsController {

    private final UserDetailsService userDetailsService;

    @RequestMapping("/{userId}")
    public ResponseEntity<UserDetailsResponse> getUserDetails(
            @CurrentUserId UUID currentUserId,
            @PathVariable UUID userId
    ) {
        UserDetailsResponse userDetails = userDetailsService.getUserDetails(currentUserId, userId);
        return ResponseEntity.ok(userDetails);
    }

}
