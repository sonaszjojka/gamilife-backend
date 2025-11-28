package edu.pjwstk.notification.controller;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.notification.dto.NotificationDto;
import edu.pjwstk.notification.enums.NotificationType;
import edu.pjwstk.notification.service.NotificationService;
import lombok.AllArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@AllArgsConstructor
@RequestMapping("/test-ws")
@Deprecated(forRemoval = true)
class TestController {

    private final NotificationService notificationService;
    private final AuthApi authApi;

    @GetMapping
    public void test() {
        CurrentUserDto user = authApi.getCurrentUser();
        NotificationDto notificationDto = NotificationDto.builder()
                .title("Test notification")
                .message("Test description of notification")
                .notificationType(NotificationType.OTHER)
                .build();
        notificationService.sendUserNotification(user.userId(), notificationDto);
    }

}
