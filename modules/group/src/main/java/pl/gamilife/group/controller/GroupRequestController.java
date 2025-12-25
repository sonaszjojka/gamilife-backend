package pl.gamilife.group.controller;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.group.controller.request.EditGroupRequestStatusForGroupRequestRequest;
import pl.gamilife.group.controller.response.ApiResponse;
import pl.gamilife.group.usecase.creategrouprequest.CreateGroupRequestCommand;
import pl.gamilife.group.usecase.creategrouprequest.CreateGroupRequestResult;
import pl.gamilife.group.usecase.creategrouprequest.CreateGroupRequestUseCase;
import pl.gamilife.group.usecase.deletegrouprequest.DeleteGroupRequestCommand;
import pl.gamilife.group.usecase.deletegrouprequest.DeleteGroupRequestUseCase;
import pl.gamilife.group.usecase.editgrouprequeststatusforgrouprequest.EditGroupRequestStatusForGroupRequestCommand;
import pl.gamilife.group.usecase.editgrouprequeststatusforgrouprequest.EditGroupRequestStatusForGroupRequestResult;
import pl.gamilife.group.usecase.editgrouprequeststatusforgrouprequest.EditGroupRequestStatusForGroupRequestUseCase;
import pl.gamilife.group.usecase.getgrouprequests.GetGroupRequestsCommand;
import pl.gamilife.group.usecase.getgrouprequests.GetGroupRequestsResult;
import pl.gamilife.group.usecase.getgrouprequests.GetGroupRequestsUseCase;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/groups/{groupId}/requests")
public class GroupRequestController {

    private final CreateGroupRequestUseCase createGroupRequestUseCase;
    private final DeleteGroupRequestUseCase deleteGroupRequestUseCase;
    private final EditGroupRequestStatusForGroupRequestUseCase editGroupRequestStatusForGroupRequestUseCase;
    private final GetGroupRequestsUseCase getGroupRequestsUseCase;


    @PostMapping
    public ResponseEntity<CreateGroupRequestResult> save(@PathVariable("groupId") UUID groupId) {
        CreateGroupRequestResult response = createGroupRequestUseCase.execute(
                new CreateGroupRequestCommand(groupId)
        );
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping("/{groupRequestId}")
    public ResponseEntity<ApiResponse> deleteById(@PathVariable("groupRequestId") UUID groupRequestId,
                                                  @PathVariable("groupId") UUID groupId) {
        deleteGroupRequestUseCase.execute(new DeleteGroupRequestCommand(
                groupId,
                groupRequestId
        ));
        return ResponseEntity.ok(
                new ApiResponse("Group request with id: " + groupRequestId + " deleted successfully.")
        );
    }

    @PutMapping("/{groupRequestId}/status")
    public ResponseEntity<EditGroupRequestStatusForGroupRequestResult> editStatusById(
            @PathVariable("groupRequestId") UUID groupRequestId,
            @PathVariable("groupId") UUID groupId,
            @Valid @RequestBody EditGroupRequestStatusForGroupRequestRequest request
    ) {
        EditGroupRequestStatusForGroupRequestResult response = editGroupRequestStatusForGroupRequestUseCase.execute(
                new EditGroupRequestStatusForGroupRequestCommand(
                        groupId,
                        groupRequestId,
                        request.groupRequestStatusId()
                )
        );
        return ResponseEntity.ok(response);
    }

    @GetMapping
    public ResponseEntity<GetGroupRequestsResult> getAllGroupRequests(
            @PathVariable("groupId") UUID groupId,
            @RequestParam(required = false) Integer statusId,
            @RequestParam(defaultValue = "0") @Min(0) Integer page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(100) Integer size) {

        GetGroupRequestsResult response = getGroupRequestsUseCase.execute(
                new GetGroupRequestsCommand(
                        groupId,
                        statusId,
                        page,
                        size
                )
        );
        return ResponseEntity.ok(response);
    }

}
