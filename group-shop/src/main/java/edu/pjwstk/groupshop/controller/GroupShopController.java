package edu.pjwstk.groupshop.controller;

import edu.pjwstk.groupshop.usecase.creategroupshop.CreateGroupShopRequest;
import edu.pjwstk.groupshop.usecase.creategroupshop.CreateGroupShopResponse;
import edu.pjwstk.groupshop.usecase.creategroupshop.CreateGroupShopUseCase;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/shop")
public class GroupShopController {
    private final CreateGroupShopUseCase createGroupUseCase;


    public GroupShopController(CreateGroupShopUseCase createGroupUseCase) {
        this.createGroupUseCase = createGroupUseCase;
    }

    @PostMapping()
    public ResponseEntity<CreateGroupShopResponse> createGroupShop(@PathVariable(name="groupId") UUID groupId, @RequestBody CreateGroupShopRequest request) {
       CreateGroupShopResponse response= createGroupUseCase.execute(request, groupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}
