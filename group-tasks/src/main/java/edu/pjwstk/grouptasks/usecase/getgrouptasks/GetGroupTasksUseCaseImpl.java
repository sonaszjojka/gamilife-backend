package edu.pjwstk.grouptasks.usecase.getgrouptasks;

import pl.gamilife.api.task.TasksApi;
import pl.gamilife.api.task.dto.TaskDto;
import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.repository.jpa.GroupTaskMemberRepositoryJpa;
import edu.pjwstk.grouptasks.repository.jpa.GroupTaskRepositoryJpa;
import edu.pjwstk.grouptasks.util.GroupTasksSpecificationBuilder;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.UUID;

@Component
public class GetGroupTasksUseCaseImpl implements GetGroupTasksUseCase {
    private final GroupTaskRepositoryJpa groupTaskRepository;
    private final GroupTaskMemberRepositoryJpa groupTaskMemberRepository;
    private final GroupTasksSpecificationBuilder specificationBuilder;
    private final TasksApi tasksApi;

    public GetGroupTasksUseCaseImpl(GroupTaskRepositoryJpa groupTaskRepository, GroupTaskMemberRepositoryJpa groupTaskMemberRepository, GroupTasksSpecificationBuilder specificationBuilder, TasksApi tasksApi) {
        this.groupTaskRepository = groupTaskRepository;
        this.groupTaskMemberRepository = groupTaskMemberRepository;
        this.specificationBuilder = specificationBuilder;
        this.tasksApi = tasksApi;
    }

    @Override
    public Page<GetGroupTaskDto> execute(UUID groupId,GetGroupTasksRequestFilter request) {

        Specification<GroupTask> groupTaskSpecification = specificationBuilder.build(
                groupId,
                request.isAccepted()
        );

        Pageable pageable = createPageable(request);

        Page<GetGroupTaskDto> tasks = groupTaskRepository.findAll(groupTaskSpecification,pageable).map(
                groupTask ->{
                    TaskDto taskDto = tasksApi.findTaskByTaskId(groupTask.getTaskId());
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
                            taskDto,
                            groupTaskMembers
                    );

                });
        return tasks;
    }

    private Pageable createPageable(GetGroupTasksRequestFilter request) {

        return PageRequest.of(
                request.pageNumber(),
                request.pageSize()
        );
    }


}
