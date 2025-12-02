package pl.gamilife.group.controller;

import pl.gamilife.group.usecase.getgrouptypes.GetGroupTypesCommand;
import pl.gamilife.group.usecase.getgrouptypes.GetGroupTypesResult;
import pl.gamilife.group.usecase.getgrouptypes.GetGroupTypesUseCase;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/group-types")
public class GroupTypeController {

    private final GetGroupTypesUseCase getGroupTypesUseCase;

    @GetMapping
    ResponseEntity<List<GetGroupTypesResult>> getAllGroupTypes(){
        return ResponseEntity.ok(getGroupTypesUseCase.execute(new GetGroupTypesCommand()));
    }
}
