package pl.gamilife.pomodoro.infrastructure.web.request;


import jakarta.validation.constraints.Positive;

public record EditPomodoroItemRequest(
        @Positive
        Integer cyclesRequired,

        @Positive
        Integer completeCycles,

        Boolean completed
) {

}
