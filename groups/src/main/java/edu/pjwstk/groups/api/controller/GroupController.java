package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.usecase.creategroup.CreateGroupRequest;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupResponse;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/groups")
public class GroupController {

    private final CreateGroupUseCase createGroupUseCase;

    public GroupController(CreateGroupUseCase createGroupUseCase) {
        this.createGroupUseCase = createGroupUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateGroupResponse> save(@RequestBody @Valid CreateGroupRequest request) {
        CreateGroupResponse response = createGroupUseCase.execute(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}
