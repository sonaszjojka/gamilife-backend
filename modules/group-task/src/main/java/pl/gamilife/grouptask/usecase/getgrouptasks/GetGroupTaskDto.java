package pl.gamilife.grouptask.usecase.getgrouptasks;

import lombok.Builder;
import pl.gamilife.api.task.dto.TaskDto;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

@Builder
public record GetGroupTaskDto(

        UUID groupTaskId,
        Integer reward,
        Instant acceptedDate,
        TaskDto taskDto,
        List<GetGroupTaskMemberDto> groupTaskMembers

) {
}
