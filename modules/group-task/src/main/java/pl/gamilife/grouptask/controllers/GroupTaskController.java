package pl.gamilife.grouptask.controllers;

import pl.gamilife.grouptask.shared.ApiResponse;
import pl.gamilife.grouptask.usecase.creategrouptask.CreateGroupTaskRequest;
import pl.gamilife.grouptask.usecase.creategrouptask.CreateGroupTaskResponse;
import pl.gamilife.grouptask.usecase.creategrouptask.CreateGroupTaskUseCase;
import pl.gamilife.grouptask.usecase.deletegrouptask.DeleteGroupTaskUseCase;
import pl.gamilife.grouptask.usecase.editgrouptask.EditGroupTaskRequest;
import pl.gamilife.grouptask.usecase.editgrouptask.EditGroupTaskResponse;
import pl.gamilife.grouptask.usecase.editgrouptask.EditGroupTaskUseCase;
import pl.gamilife.grouptask.usecase.getgrouptasks.GetGroupTaskDto;
import pl.gamilife.grouptask.usecase.getgrouptasks.GetGroupTasksRequestFilter;
import pl.gamilife.grouptask.usecase.getgrouptasks.GetGroupTasksUseCase;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/tasks")
public class GroupTaskController {
    private final CreateGroupTaskUseCase createGroupTaskUseCase;
    private final DeleteGroupTaskUseCase deleteGroupTaskUseCase;
    private final EditGroupTaskUseCase editGroupTaskUseCase;
    private final GetGroupTasksUseCase getGroupTasksUseCase;
    public GroupTaskController(CreateGroupTaskUseCase createGroupTaskUseCase, DeleteGroupTaskUseCase deleteGroupTaskUseCase, EditGroupTaskUseCase editGroupTaskUseCase, GetGroupTasksUseCase getGroupTasksUseCase) {
        this.createGroupTaskUseCase = createGroupTaskUseCase;
        this.deleteGroupTaskUseCase = deleteGroupTaskUseCase;
        this.editGroupTaskUseCase = editGroupTaskUseCase;
        this.getGroupTasksUseCase = getGroupTasksUseCase;
    }

    @PostMapping("")
    public ResponseEntity<CreateGroupTaskResponse> save (@PathVariable ("groupId") UUID groupId,
                                                         @RequestBody @Valid CreateGroupTaskRequest request) {

        CreateGroupTaskResponse response= createGroupTaskUseCase.execute(request, groupId);

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping("/{groupTaskId}")
    public ResponseEntity<ApiResponse> delete (@PathVariable ("groupTaskId") UUID groupTaskId) {

        deleteGroupTaskUseCase.execute(groupTaskId);

        return ResponseEntity.status(HttpStatus.NO_CONTENT).body(new ApiResponse("Group Task with id: " + groupTaskId + " deleted successfully"));
    }

    @PutMapping("/{groupTaskId}")
    public ResponseEntity<EditGroupTaskResponse> edit (@PathVariable ("groupTaskId") UUID groupTaskId,
                                                       @RequestBody @Valid EditGroupTaskRequest request) {

        EditGroupTaskResponse response = editGroupTaskUseCase.execute(groupTaskId, request);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @GetMapping("")
    public ResponseEntity<Page<GetGroupTaskDto>> get (@PathVariable ("groupId") UUID groupId,
                                                      @RequestParam(required = false) Boolean isAccepted,
                                                      @RequestParam(defaultValue ="0") @Min(0) Integer page,
                                                      @RequestParam(defaultValue ="10") @Min(1) Integer size) {
        GetGroupTasksRequestFilter filter = new GetGroupTasksRequestFilter(isAccepted, page, size);

        Page<GetGroupTaskDto> response = getGroupTasksUseCase.execute(groupId, filter);
        return ResponseEntity.status(HttpStatus.OK).body(response);

    }

}
