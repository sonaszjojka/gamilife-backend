package pl.gamilife.communication.controller;

import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.communication.dto.NotificationDto;
import pl.gamilife.communication.enums.NotificationType;
import pl.gamilife.communication.usecase.sendusernotification.SendUserNotificationCommand;
import pl.gamilife.communication.usecase.sendusernotification.SendUserNotificationUseCase;
import lombok.AllArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@AllArgsConstructor
@RequestMapping("/test-ws")
@Deprecated(forRemoval = true)
class TestController {

    private final AuthApi authApi;
    private final SendUserNotificationUseCase sendUserNotificationUseCase;

    @GetMapping
    public void test() {
        CurrentUserDto user = authApi.getCurrentUser();
        NotificationDto notificationDto = NotificationDto.builder()
                .title("Test notification")
                .message("Test description of notification")
                .notificationType(NotificationType.OTHER)
                .build();
        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                user.userId(),
                notificationDto
        ));
    }

}
