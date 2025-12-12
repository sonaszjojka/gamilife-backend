package pl.gamilife.grouptask.usecase.getgrouptasks;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.repository.jpa.GroupTaskMemberRepositoryJpa;
import pl.gamilife.grouptask.repository.jpa.GroupTaskRepositoryJpa;
import pl.gamilife.grouptask.util.GroupTasksSpecificationBuilder;

import java.util.List;
import java.util.UUID;

@Component
public class GetGroupTasksUseCaseImpl implements GetGroupTasksUseCase {
    private final GroupTaskRepositoryJpa groupTaskRepository;
    private final GroupTaskMemberRepositoryJpa groupTaskMemberRepository;
    private final GroupTasksSpecificationBuilder specificationBuilder;
    private final TaskApi taskApi;

    public GetGroupTasksUseCaseImpl(GroupTaskRepositoryJpa groupTaskRepository, GroupTaskMemberRepositoryJpa groupTaskMemberRepository, GroupTasksSpecificationBuilder specificationBuilder, TaskApi taskApi) {
        this.groupTaskRepository = groupTaskRepository;
        this.groupTaskMemberRepository = groupTaskMemberRepository;
        this.specificationBuilder = specificationBuilder;
        this.taskApi = taskApi;
    }

    @Override
    public Page<GetGroupTaskDto> execute(UUID groupId, GetGroupTasksRequestFilter request) {

        Specification<GroupTask> groupTaskSpecification = specificationBuilder.build(
                groupId,
                request.isAccepted(),
                request.isDeclined()

        );

        Pageable pageable = createPageable(request);

        return groupTaskRepository.findAll(groupTaskSpecification, pageable).map(
                groupTask -> {
                    TaskDto taskDto = taskApi.findTaskById(groupTask.getTaskId());
                    List<GetGroupTaskMemberDto> groupTaskMembers =
                            groupTaskMemberRepository.findByGroupTaskId(groupTask)
                                    .stream()
                                    .map(g -> new GetGroupTaskMemberDto(
                                            g.getGroupTaskMemberId(),
                                            g.getGroupMemberId(),
                                            g.getIsMarkedDone()
                                    ))
                                    .toList();

                    return new GetGroupTaskDto(
                            groupTask.getGroupTaskId(),
                            groupTask.getReward(),
                            groupTask.getAcceptedDate(),
                            groupTask.getDeclineMessage(),
                            taskDto,
                            groupTaskMembers
                    );

                });
    }

    private Pageable createPageable(GetGroupTasksRequestFilter request) {

        return PageRequest.of(
                request.pageNumber(),
                request.pageSize()
        );
    }


}
