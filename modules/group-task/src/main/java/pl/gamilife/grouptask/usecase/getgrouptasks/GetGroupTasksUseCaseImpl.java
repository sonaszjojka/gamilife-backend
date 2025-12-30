package pl.gamilife.grouptask.usecase.getgrouptasks;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.entity.GroupTaskMember;
import pl.gamilife.grouptask.repository.jpa.GroupTaskMemberRepositoryJpa;
import pl.gamilife.grouptask.repository.jpa.GroupTaskRepositoryJpa;
import pl.gamilife.grouptask.util.GroupTasksSpecificationBuilder;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.List;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetGroupTasksUseCaseImpl implements GetGroupTasksUseCase {
    private final GroupTaskRepositoryJpa groupTaskRepository;
    private final GroupTaskMemberRepositoryJpa groupTaskMemberRepository;
    private final GroupTasksSpecificationBuilder specificationBuilder;
    private final TaskApi taskApi;

    @Override
    public Page<GetGroupTaskDto> execute(UUID groupId, GetGroupTasksRequestFilter request) {
        Specification<GroupTask> groupTaskSpecification = specificationBuilder.build(
                groupId,
                request.isAccepted(),
                request.isDeclined()
        );

        Pageable pageable = createPageable(request);
        var rawResult = groupTaskRepository.findAll(groupTaskSpecification, pageable);
        Page<GroupTask> result = new Page<>(
                rawResult.getContent(),
                rawResult.getTotalElements(),
                rawResult.getTotalPages(),
                rawResult.getNumber(),
                rawResult.getSize()
        );

        var taskIds = result.content().stream().map(GroupTask::getTaskId).toList();
        var tasksByIds = taskApi.findTasksByIds(taskIds).stream()
                .collect(Collectors.toMap(TaskDto::id, Function.identity()));

        var groupTaskIds = result.content().stream().map(GroupTask::getId).toList();
        var groupTaskMembers = groupTaskMemberRepository.findByGroupTaskIdIn(groupTaskIds)
                .stream()
                .collect(Collectors.groupingBy(GroupTaskMember::getGroupTaskId));

        return result.map(groupTask -> new GetGroupTaskDto(
                        groupTask.getId(),
                        groupTask.getReward(),
                        groupTask.getAcceptedAt(),
                        groupTask.getDeclineMessage(),
                        tasksByIds.get(groupTask.getTaskId()),
                        groupTaskMembers.get(groupTask.getId()) != null
                                ? groupTaskMembers.get(groupTask.getId()).stream()
                                .map(gtm -> new GetGroupTaskMemberDto(
                                        gtm.getId(),
                                        gtm.getGroupMemberId(),
                                        gtm.getMarkedDoneAt() != null
                                )).toList()
                                : List.of()
                )
        );
    }

    private Pageable createPageable(GetGroupTasksRequestFilter request) {

        return PageRequest.of(
                request.pageNumber(),
                request.pageSize()
        );
    }

}
