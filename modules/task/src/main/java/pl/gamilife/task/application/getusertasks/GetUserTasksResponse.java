package pl.gamilife.task.application.getusertasks;


import lombok.Builder;

import java.util.List;


@Builder
public record GetUserTasksResponse(
        List<GetUserTasksDto> tasks

) {

}
